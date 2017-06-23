{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiWayIf, NondecreasingIndentation, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Looper where

import Prelude hiding (break, (>>), id, (.))
import Control.Category

import qualified Control.Exception as Exc
import qualified Control.Monad as M
import qualified Data.Time.Clock as Clock
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO


import Control.Arrow (second)
import Control.Monad (forM, forM_, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, gets, put, modify, runStateT)
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid (Monoid(..), (<>))

import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

import qualified APC40mkII_Raw as APC
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Array as Array
import qualified Foreign.C.String as C
import qualified PureLoop as Loop
import qualified Sequencer as S
import Control


foreign export ccall hs_looper_init :: IO (StablePtr LooperState)
foreign export ccall hs_looper_main 
    :: StablePtr LooperState
    -> Word32                          -- window size
    -> Word32                          -- input channels
    -> Word32                          -- output channels
    -> Foreign.Ptr (Foreign.Ptr Float) -- channel data
    -> IO ()
foreign export ccall hs_looper_uilog :: StablePtr LooperState -> IO C.CString
foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()

type MVector = MVector.MVector RealWorld

newtype ControlIn = ControlIn { getControlIn :: APC.APCOutMessage }
    deriving (Eq)
newtype ControlOut = ControlOut { getControlOut :: APC.APCInMessage }
    deriving (Eq)

fromAPC :: APC.APCInMessage -> ControlOut
fromAPC = ControlOut

toAPC :: (Monad m) => ControlIn -> m APC.APCOutMessage
toAPC (ControlIn m) = return m

type LooperM = S.SequencerT ControlIn ControlOut (StateT ControlState IO)


data InputChannel = InputChannel
    { icLevel :: Double
    }


data ControlState = ControlState
    { csChannels :: Array.Array Int Channel
    , csQuantization :: Maybe (TimeDiff, Time)   -- period, phase  (i.e. (period*n + phase) is a barline)
    , csLoops :: Array.Array APC.Coord Loop.Loop
    , csQueue :: [(Maybe APC.Coord, Transition ControlState ())]  -- reversed
    , csLevel :: Double
    , csBreak :: Bool
    }

data ActiveSlotState = Recording | Playing Time -- phase
    deriving (Eq, Show)

stretchActiveSlot :: Double -> Time -> ActiveSlotState -> ActiveSlotState
stretchActiveSlot stretch pos (Playing phase) = Playing (stretchPhase stretch pos phase)
stretchActiveSlot _ _ a = a

-- g x = f (x / s)
-- 
-- f (pos - phase) = g (pos - phase')
--                 = f ((pos - phase') / s)
-- pos - phase = (pos - phase')/s
-- pos - phase = pos/s - phase'/s
-- phase'/s = pos/s - pos + phase
-- phase' = s(pos/s - pos + phase)
--        = pos - pos*s + phase*s
--        = pos + s*(phase - pos)
stretchPhase :: Double -> Time -> Time -> Time
stretchPhase stretch pos phase = pos ^+^ (stretch *^ (phase ^-^ pos))

data Channel = Channel
    { chSlots :: Array.Array Int Bool
    , chActiveSlot :: Maybe (Int, ActiveSlotState)
    , chLevel :: Double
    , chMute  :: Bool
    }


newtype MonadMonoid m = MonadMonoid { runMonadMonoid :: m () }

instance (Monad m) => Monoid (MonadMonoid m) where
    mempty = MonadMonoid (return ())
    mappend (MonadMonoid m) (MonadMonoid m') = MonadMonoid (m >> m')


newtype Transition s a = Transition { runTransition :: RWS.RWS Time (MonadMonoid LooperM, MonadMonoid LooperM) s a }
    deriving (Functor, Applicative, Monad)

instance Monoid (Transition s ()) where
    mempty = return ()
    mappend a b = a >> b

transLights :: LooperM () -> Transition s ()
transLights m = Transition $ RWS.tell (MonadMonoid m, mempty)

transActions :: LooperM () -> Transition s ()
transActions m = Transition $ RWS.tell (mempty, MonadMonoid m)

-- XXX todo some concept duplication here, use MonadState?
transModify :: (s -> s) -> Transition s ()
transModify = Transition . RWS.modify

transPut :: s -> Transition s ()
transPut = Transition . RWS.put

transGet :: Transition s s
transGet = Transition RWS.get

transGets :: (s -> a) -> Transition s a
transGets = Transition . RWS.gets

transTime :: Transition s Time
transTime = Transition RWS.ask

rwsMapState :: (Functor m) => (s' -> s) -> (s -> s' -> s') -> RWS.RWST r w s m a -> RWS.RWST r w s' m a
rwsMapState getf putf m = RWS.RWST $ \r s -> 
    fmap (\(a, s', w) -> (a, putf s' s, w)) (RWS.runRWST m r (getf s))

transMapState :: (s' -> s) -> (s -> s' -> s') -> Transition s a -> Transition s' a
transMapState getf putf = Transition . rwsMapState getf putf . runTransition


playingColor,recordingColor,stoppedColor,offColor :: APC.RGBColorState 
playingColor   = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)
recordingColor = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7)
stoppedColor   = APC.RGBSolid (APC.velToRGBColor 14)
offColor       = APC.RGBOff


transChannel :: Int -> Transition Channel a -> Transition ControlState a
transChannel i = transMapState ((Array.! i) . csChannels) (\ch' s -> s { csChannels = csChannels s Array.// [(i,ch')] })

transAllChannels :: Transition Channel () -> Transition ControlState ()
transAllChannels t = do
    channels <- transGets csChannels
    forM_ (Array.indices channels) $ \i -> transChannel i t

restartChannel :: Time -> Transition Channel ()
restartChannel phase = transModify $ \ch -> 
    ch { chActiveSlot = fmap (\(n, slotstate) ->
            (n, case slotstate of
                    Playing _ -> Playing phase
                    Recording -> Recording)) (chActiveSlot ch) }

matches :: [a] -> Bool
matches = not . null

startLooper :: LooperM ()
startLooper = do
    -- setup state
    lift $ do
        let loops = Array.array (APC.Coord (1,1), APC.Coord (8,5)) $
                      [ (APC.Coord (i,j), mempty) | i <- [1..8], j <- [1..5] ]
        put $ ControlState
            { csChannels = Array.listArray (1,8) . replicate 8 $
                Channel { chSlots = Array.listArray (1,5) . replicate 5 $ False
                        , chActiveSlot = Nothing
                        , chLevel = 1
                        , chMute = False
                        }
            , csQuantization = Nothing
            , csLoops = loops
            , csQueue = []
            , csLevel = 1
            , csBreak = False
            } 

    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressDown <- toAPC e
        lift . schedule . (Just (APC.Coord (i,j)),) $
            transChannel i . tapChannel i j =<< transTime
    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressLong <- toAPC e
        lift . activateTransition $ transChannel i . (stopActive i >>) $ do
            transModify (\ch -> ch { chSlots = chSlots ch Array.// [(j, False)] })
            transLights . S.send . fromAPC $ APC.InMatrixButton (APC.Coord (i,j)) offColor
            transActions $ do
                do freshLoop i j
                   clearQueue (APC.Coord (i,j))
    S.whenever $ \e -> do
        APC.OutFader i level <- toAPC e
        lift $
            if | 1 <= i && i <= 8 -> 
                activateTransition . transChannel i $ 
                    transModify (\ch -> ch { chLevel = level })
               | i == 9 ->
                activateTransition $
                    transModify (\cs -> cs { csLevel = level })
               | otherwise ->
                    return ()
    S.whenever $ \e -> do
        APC.OutTempoChange dt <- toAPC e
        lift $ do
            activateTransition $ do
                time <- transTime
                let stretch = 1.01^^(-dt)
                transModify (\s -> s
                   { csLoops = csLoops s -- fmap (Loop.stretch stretch) (csLoops s)  -- TODO re-enable stretch
                   , csQuantization = fmap (\(period, phase) -> 
                        (stretch *^ period, stretchPhase stretch time phase)) (csQuantization s)
                   , csChannels = fmap (\ch -> ch { 
                        chActiveSlot = (fmap.second) (stretchActiveSlot stretch time) (chActiveSlot ch) }) (csChannels s)
                   })
    S.whenever $ \e -> do
        APC.OutUnmuteButton ch True <- toAPC e
        lift . activateTransition . transChannel ch $ do
            s <- transGet
            transPut (s { chMute = not (chMute s) })
            transLights . S.send . fromAPC $ APC.InUnmuteButton ch (chMute s)
    S.whenever $ \e -> do
        APC.OutStopAllButton True <- toAPC e
        lift $ do
            quant <- lift $ gets csQuantization
            break <- lift $ gets csBreak
            let delay | Just (bar, _) <- quant, not break = bar ^/ 16
                      | otherwise                         = zeroV
            after delay . activateTransition $ do
                state <- transGet
                if not (csBreak state) then
                    transModify (\s -> s { csBreak = True })
                else do
                    time <- transTime
                    transAllChannels (restartChannel time)
                    transModify (\s -> s { csBreak = False
                                         , csQuantization = fmap (\(l,_) -> (l, time)) (csQuantization s) })
    S.whenever $ \e -> do
        APC.OutMetronome PressLong <- toAPC e
        lift . lift $ modify (\s -> s { csQuantization = Nothing })
    S.when $ \e -> do 
        APC.OutSessionButton True <- toAPC e
        -- XXX hack -- we can't reboot in the middle of a conditional event handler,
        -- because of the way Sequencer.processEvents works. It will not correctly
        -- clear conditional events.  So we use S.after 0 to convert into a timed
        -- event, which works correctly.
        lift . after zeroV $ do
            S.rebootSequencerT apc40Raw
            startLooper

freshLoop :: Int -> Int -> LooperM ()
freshLoop i j = do
    lift $ modify (\s -> s { csLoops = csLoops s Array.// [(APC.Coord (i,j), mempty)] })

clearQueue :: APC.Coord -> LooperM ()
clearQueue coord = do
    lift $ modify (\s -> s { csQueue = filter ((Just coord /=) . fst) (csQueue s) })

tapChannel :: Int -> Int -> Time -> Transition Channel ()
tapChannel i j pos = do
    ch <- transGet
    if | Just (j', Recording) <- chActiveSlot ch,
         j' == j -> do 
            transPut (ch { chActiveSlot = Just (j, Playing pos) })
            transLights . S.send . fromAPC $ APC.InMatrixButton (APC.Coord (i,j)) playingColor
            transActions $ maybeSetQuantization i j
       | Just (j', Playing _) <- chActiveSlot ch,
         j' == j -> do
            stopActive i
       | chSlots ch Array.! j -> do
            stopActive i
            transPut (ch { chActiveSlot = Just (j, Playing pos) })
            transLights $ S.send . fromAPC $ APC.InMatrixButton (APC.Coord (i,j)) playingColor
       -- not (chSlots ch Array.! j)
       | otherwise -> do
            stopActive i
            transPut (ch { chSlots = chSlots ch Array.// [(j, True)], chActiveSlot = Just (j, Recording) })
            transLights $ S.send . fromAPC $ APC.InMatrixButton (APC.Coord (i,j)) recordingColor
            transActions $ freshLoop i j

maybeSetQuantization :: Int -> Int -> LooperM ()
maybeSetQuantization i j = do
    state <- lift get
    M.when (isNothing (csQuantization state)) $ do
        let loop = csLoops state Array.! APC.Coord (i,j)
        let loopsize = fromSamples (Loop.length loop)
        setQuantization loopsize

setQuantization :: TimeDiff -> LooperM ()
setQuantization cycleLength = do
    time <- S.getCurrentTime
    lift $ modify (\s -> s { csQuantization = Just (qlength, time) })
    where
    -- If we have long loops, half/double until the tempo is reasonable.
    -- This should work for most types of music.
    qlength = until (>= minlength) (2 *^) . until (<= maxlength) (0.5 *^) $ cycleLength
    maxlength = fromSamples (44100*4)  -- 4 seconds  (60bpm)
    minlength = fromSamples 44100      -- 1 second  (240bpm)

stopActive :: Int -> Transition Channel ()
stopActive i = do
    ch <- transGet
    if | Just (j, _) <- chActiveSlot ch -> do
            transPut (ch { chActiveSlot = Nothing })
            transLights . S.send . fromAPC $ APC.InMatrixButton (APC.Coord (i,j)) stoppedColor
       | otherwise  -> return ()

schedule :: (Maybe APC.Coord, Transition ControlState ()) -> LooperM ()
schedule t = lift $ modify (\s -> s { csQueue = t : csQueue s })

activateTransition :: Transition ControlState a -> LooperM a
activateTransition t = do
    state <- lift get
    time <- S.getCurrentTime
    let (x, state', (MonadMonoid lights, MonadMonoid actions)) = RWS.runRWS (runTransition t) time state
    lift $ put state'
    lights
    actions
    return x

sumV :: Int -> [Vector.Vector Double] -> Vector.Vector Double
sumV l [] = Vector.replicate l 0
sumV _ (v:vs) = foldr (Vector.zipWith (+)) v vs

renderMix :: Vector.Vector Double -> LooperM (Vector.Vector Double)
renderMix inbuf = do
    state <- lift get
    time <- S.getCurrentTime
    let (updates, outs) = mconcat $ do
            (i,ch) <- Array.assocs (csChannels state)
            guard $ not (chMute ch)
            return $ if | Just (j, Recording) <- chActiveSlot ch ->
                            let coord = APC.Coord (i,j) in
                            ([(coord, (csLoops state Array.! coord) <> Loop.fromVector inbuf)], [])
                        | Just (j, Playing phase) <- chActiveSlot ch ->
                            let coord = APC.Coord (i,j) in
                            ([], [Loop.indexRange (toSamples (time ^-^ phase)) 
                                                  (Vector.length inbuf)
                                                  (csLoops state Array.! coord)
                                                  (csLevel state * chLevel ch)])
                        | otherwise ->
                            ([], [])
    lift $ put (state { csLoops = csLoops state Array.// updates }) 
    return $ sumV (Vector.length inbuf) outs

iterWhileM :: (Monad m) => m (Maybe a) -> m [a]
iterWhileM m = m >>= \case
    Just x -> (x:) <$> iterWhileM m
    Nothing -> return []

writeLog :: (MonadIO m) => String -> m ()
writeLog msg = liftIO $ do
    time <- Clock.getCurrentTime
    IO.withFile "/tmp/looperlog" IO.AppendMode $ \fh -> do
        IO.hPutStrLn fh $ show time ++ ": " ++ msg

runLooper :: Vector.Vector Double -> LooperM (Vector.Vector Double)
runLooper inbuf = do
    startTime <- S.getCurrentTime
    let targetTime = startTime ^+^ fromSamples (Vector.length inbuf)

    S.processMidiEvents
    outbufs <- iterWhileM $ do
        time0 <- S.getCurrentTime
        more <- S.nextEvent targetTime
        if more then do
            time1 <- S.getCurrentTime
            outbuf <- runLooper1 (Vector.slice (toSamples (time0 ^-^ startTime)) (toSamples (time1 ^-^ time0)) inbuf)
            return $ Just outbuf
        else do
            return Nothing
    let retbuf = mconcat outbufs
    M.when (Vector.length retbuf /= Vector.length inbuf) $ 
        writeLog ("Return buffer size " ++ show (Vector.length retbuf) 
            ++ " is not the same as input buffer size " ++ show (Vector.length inbuf))
    return $ mconcat outbufs
    
-- runLooper1 runs the looper on a "contiguous fragment" of the event stream -- i.e., runLooper proper
-- guarantees that for the duration of inbuf (which could be arbitrarily small) no events or state
-- changes take place.  TODO this should somehow be represented in the type.
runLooper1 :: Vector.Vector Double -> LooperM (Vector.Vector Double)
runLooper1 inbuf = do
    let winsize = Vector.length inbuf
    state <- lift get
    time <- S.getCurrentTime

    runqueue <- case (csBreak state, csQuantization state) of
        (True, _) -> return False
        (False, Nothing) -> return True
        (False, Just (period, phase)) -> do
            let bar = quantPoint winsize period phase time
            let beat = quantPoint winsize (period ^/ 4) phase time -- TODO more flexible than 4 beats/bar
            -- XXX "24 times per quarter note" but subdiv doesn't match, seems to be 12 times 
            -- per quarter according to the APC.
            let clock = quantPoint winsize (period ^/ (2*24)) phase time
            M.when clock . S.send . fromAPC $ APC.InClock
             
            if bar then do
                S.send (fromAPC (APC.InMetronome True))
                after (fromMillisec 200) $ S.send (fromAPC (APC.InMetronome False))
            else if beat then do
                S.send (fromAPC (APC.InMetronome True))
                after (fromMillisec 100) $ S.send (fromAPC (APC.InMetronome False))
            else return ()

            return bar
    M.when runqueue $ do
        let queue = csQueue state
        lift $ modify (\s -> s { csQueue = [] })
        forM_ (reverse queue) (activateTransition . snd)

    endtime <- S.getCurrentTime
    M.when (time /= endtime) $ 
        writeLog ("Start time " ++ show time ++ " is not equal to end time " ++ show endtime)
    
    -- It is important that renderMix reads the *new* state after running the queue.
    renderMix inbuf

    where
    quantPoint :: Int -> TimeDiff -> Time -> Time -> Bool
    quantPoint winsize period phase time = thisBar /= lastBar
        where
        thisBar = floor (ratio (time ^-^ phase) period) :: Int 
        lastBar = floor (ratio ((time ^-^ phase) ^-^ fromSamples winsize) period)

type IOBuffers = MVector Double

data LooperState = LooperState 
    { lsMidiDevs    :: Devs
    , lsStartTime   :: Time
    , lsFrame       :: Vector.Vector Double -> LooperM (Vector.Vector Double)
    , lsSeqState    :: IORef (S.SeqState LooperM ControlIn ControlOut, ControlState)
    , lsBuffers     :: IORef (Int, IOBuffers)
    }

apc40Raw :: MIDIControl LooperM ControlOut ControlIn
apc40Raw = right (arr ControlIn) . APC.apc40Raw . right (arr getControlOut)

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- openDevs
    let badstate = error "first order of business must be to set state"
    let startlooper = do
            () <- startLooper
            S.getCurrentTime
    ((time0, seqstate), superstate) <- 
        runStateT 
            (S.runSequencerT startlooper =<< S.bootSequencerT devs apc40Raw) 
            badstate
    seqstateref <- newIORef (seqstate, superstate)
    buffers <- newIORef =<< (256,) <$> makeBuffers 256  -- a guess, will be reinitialized if incorrect
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsStartTime = time0
                               , lsFrame = runLooper
                               , lsSeqState = seqstateref
                               , lsBuffers = buffers
                               }

hs_looper_main :: StablePtr LooperState -> Word32 -> Word32 -> Word32 -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hs_looper_main state window input output channels = wrapErrors "hs_looper_main" $ do
    looperstate <- deRefStablePtr state
    hsLooperMain looperstate (fromIntegral window) (fromIntegral input) (fromIntegral output) channels

makeBuffers :: Int -> IO IOBuffers
makeBuffers window = buf
    where
    buf = MVector.new window

getBuffers :: LooperState -> Int -> IO IOBuffers
getBuffers state window = do
    (bufwin, buffers) <- readIORef (lsBuffers state)
    if bufwin == window
        then return buffers
        else do
            newbuffers <- makeBuffers window
            writeIORef (lsBuffers state) (window, newbuffers)
            return newbuffers

hsLooperMain :: LooperState -> Int -> Int -> Int -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hsLooperMain looperstate window _inchannels _outchannels channels = do
    inbuf <- peekElemOff channels 0
    inbufarray <- getBuffers looperstate window
    forM_ [0..window-1] $ \i -> MVector.unsafeWrite inbufarray i =<< realToFrac <$> peekElemOff inbuf i

    inbufvector <- Vector.freeze inbufarray
    
    (seqstate, superstate) <- readIORef (lsSeqState looperstate)
    ((outvector, seqstate'), superstate') 
        <- runStateT (S.runSequencerT (lsFrame looperstate inbufvector) seqstate) superstate
    writeIORef (lsSeqState looperstate) (seqstate', superstate')

    mainoutbuf <- peekElemOff channels 0
    
    forM_ [0..window-1] $ \i -> do
        let sample = realToFrac (outvector Vector.! i)
        pokeElemOff mainoutbuf i sample

hs_looper_uilog :: StablePtr LooperState -> IO C.CString
hs_looper_uilog state = wrapErrors "hs_looper_uilog" $ do
    looperstate <- deRefStablePtr state
    C.newCString =<< hsLooperUILog looperstate  -- MUST BE free()d BY CLIENT

hsLooperUILog :: LooperState -> IO String
hsLooperUILog lstate = do 
    state <- snd <$> readIORef (lsSeqState lstate)
    maybelines <- forM (Array.assocs (csChannels state)) $ \(i,ch) ->
        if | Just (j, chstate) <- chActiveSlot ch -> do
                let loopsize = show (Loop.length (csLoops state Array.! APC.Coord (i,j)))
                return . Just $ loopsize ++ " " ++ show chstate
           | otherwise -> return Nothing
    return . unlines $
        [ "quantization: " ++ show (csQuantization state) ]
        ++ catMaybes maybelines


hs_looper_exit :: StablePtr LooperState -> IO ()
hs_looper_exit state = wrapErrors "hs_looper_exit" $ do
    looperstate <- deRefStablePtr state
    closeDevs (lsMidiDevs looperstate)
    freeStablePtr state


wrapErrors :: String -> IO a -> IO a
wrapErrors entry action = Exc.catch action $ \(e :: Exc.SomeException) -> do
    writeLog $ "FATAL " ++ entry ++ " : " ++ show e
    Exc.throwIO e

(>>) :: (Monad m) => m () -> m a -> m a
a >> b = a >>= \() -> b
