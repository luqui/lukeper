{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiWayIf, NondecreasingIndentation, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Looper where

import Prelude hiding (break, (>>))

import qualified Control.Exception as Exc
import qualified Control.Monad as M
import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO

import Control.Arrow (second)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, gets, put, modify, runStateT)
import Control.Monad.Trans.Writer (Writer, tell, runWriter)
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid (Monoid(..))

import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

import qualified APC40mkII_Raw as APC
import qualified Foreign.C.String as C
import qualified Loop as Loop
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

type ControlIn = APC.APCOutMessage
type ControlOut = APC.APCInMessage
type LooperM = S.SequencerT ControlIn ControlOut (StateT ControlState IO)


type Sends = Array.Array Int Float

data MixChannel = MixChannel
    { mcLoop :: Loop.Loop
    , mcLevel :: Double
    , mcState :: ActiveSlotState
    , mcSends :: Sends
    }

data InputChannel = InputChannel
    { icLevel :: Double
    , icSends :: Sends
    }

data Mix = Mix Time InputChannel [MixChannel]


data ControlState = ControlState
    { csChannels :: Array.Array Int Channel
    , csPosition :: Int
    , csQuantization :: Maybe (Int, Int)   -- period, phase  (i.e. (period*n + phase) is a barline)
    , csLoops :: Array.Array APC.Coord Loop.Loop
    , csQueue :: [(Maybe APC.Coord, Transition ControlState ())]  -- reversed
    , csLevel :: Double
    , csBreak :: Bool
    , csSends :: Sends
    }

data ActiveSlotState = Recording | Playing Int -- int is phase
    deriving (Eq, Show)

stretchActiveSlot :: Double -> Int -> ActiveSlotState -> ActiveSlotState
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
--        = pos*(1-s) + phase*s
stretchPhase :: Double -> Int -> Int -> Int
stretchPhase stretch pos phase = round (fromIntegral pos * (1 - stretch) + fromIntegral phase * stretch)

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


newtype Transition s a = Transition { runTransition :: StateT s (Writer (MonadMonoid LooperM, MonadMonoid LooperM)) a }
    deriving (Functor, Applicative, Monad)

instance Monoid (Transition s ()) where
    mempty = return ()
    mappend a b = a >> b

transLights :: LooperM () -> Transition s ()
transLights m = Transition . lift $ tell (MonadMonoid m, mempty)

transActions :: LooperM () -> Transition s ()
transActions m = Transition . lift $ tell (mempty, MonadMonoid m)


playingColor,recordingColor,stoppedColor,offColor :: APC.RGBColorState 
playingColor   = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)
recordingColor = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7)
stoppedColor   = APC.RGBSolid (APC.velToRGBColor 14)
offColor       = APC.RGBOff


transChannel :: Int -> Transition Channel a -> Transition ControlState a
transChannel i t = Transition . StateT $ \s -> do
        (x, ch') <- runStateT (runTransition t) (csChannels s Array.! i)
        return (x, s { csChannels = csChannels s Array.// [(i,ch')] })

transAllChannels :: Transition Channel () -> Transition ControlState ()
transAllChannels t = Transition $ do
    channels <- gets csChannels
    forM_ (Array.indices channels) $ \i -> runTransition (transChannel i t)

restartChannel :: Int -> Transition Channel ()
restartChannel phase = Transition . modify $ \ch -> 
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
        loops <- Array.array (APC.Coord (1,1), APC.Coord (8,5)) <$> 
                 sequence [ (APC.Coord (i,j),) <$> liftIO Loop.newLoop | i <- [1..8], j <- [1..5] ]
        put $ ControlState
            { csChannels = Array.listArray (1,8) . replicate 8 $
                Channel { chSlots = Array.listArray (1,5) . replicate 5 $ False
                        , chActiveSlot = Nothing
                        , chLevel = 1
                        , chMute = False
                        }
            , csPosition = 0
            , csQuantization = Nothing
            , csLoops = loops
            , csQueue = []
            , csLevel = 1
            , csBreak = False
            , csSends = Array.listArray (1,8) (repeat 0)
            } 

    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressDown <- return e
        lift . schedule . (Just (APC.Coord (i,j)),) . Transition $ do
            pos <- gets csPosition 
            runTransition $ transChannel i (tapChannel i j pos)
    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressLong <- return e
        lift . activateTransition $ transChannel i . (stopActive i >>) . Transition $ do
            modify (\ch -> ch { chSlots = chSlots ch Array.// [(j, False)] })
            runTransition . transLights $ S.send (APC.InMatrixButton (APC.Coord (i,j)) offColor)
            runTransition . transActions $ do
                do freshLoop i j
                   clearQueue (APC.Coord (i,j))
    S.whenever $ \e -> do
        APC.OutFader i level <- return e
        lift $
            if | 1 <= i && i <= 8 -> 
                activateTransition . transChannel i . Transition $ 
                    modify (\ch -> ch { chLevel = level })
               | i == 9 ->
                activateTransition . Transition $
                    modify (\cs -> cs { csLevel = level })
               | otherwise ->
                    return ()
    S.whenever $ \e -> do
        APC.OutTempoChange dt <- return e
        lift $ do
            pos <- lift $ gets csPosition
            activateTransition . Transition $ do
                let stretch = 1.01^^(-dt)
                modify (\s -> s
                   { csLoops = fmap (Loop.stretch stretch) (csLoops s)
                   , csQuantization = fmap (\(period, phase) -> 
                        (round (fromIntegral period * stretch), stretchPhase stretch pos phase)) (csQuantization s)
                   , csChannels = fmap (\ch -> ch { 
                        chActiveSlot = (fmap.second) (stretchActiveSlot stretch pos) (chActiveSlot ch) }) (csChannels s)
                   })
    S.whenever $ \e -> do
        APC.OutUnmuteButton ch True <- return e
        lift . activateTransition . transChannel ch . Transition $ do
            s <- get
            put (s { chMute = not (chMute s) })
            runTransition . transLights $ S.send (APC.InUnmuteButton ch (chMute s))
    S.whenever $ \e -> do
        APC.OutStopAllButton True <- return e
        lift $ do
            quant <- lift $ gets csQuantization
            break <- lift $ gets csBreak
            let delay | Just (bar, _) <- quant, not break = fromMillisec ((1000*fromIntegral bar) / (16*44100)) -- wait 1 sixteenth to break for a final hit
                      | otherwise                         = fromMillisec 0
            after delay . activateTransition . Transition $ do
                state <- get
                if not (csBreak state) then
                    modify (\s -> s { csBreak = True })
                else do
                    runTransition $ transAllChannels (restartChannel (csPosition state))
                    modify (\s -> s { csBreak = False
                                    , csQuantization = fmap (\(l,_) -> (l, csPosition s)) (csQuantization s) })
    S.whenever $ \e -> do
        APC.OutDial dial val <- return e
        lift $ do
            S.send (APC.InDial dial val)
            lift $ modify (\s -> s { csSends = csSends s Array.// [(dial, fromIntegral val / 127)] })
    S.when $ \e -> do 
        APC.OutSessionButton True <- return e
        -- XXX hack -- we can't reboot in the middle of a conditional event handler,
        -- because of the way Sequencer.processEvents works. It will not correctly
        -- clear conditional events.  So we use S.after 0 to convert into a timed
        -- event, which works correctly.
        lift . after (fromMillisec 0) $ do
            S.rebootSequencerT APC.apc40Raw
            startLooper

freshLoop :: Int -> Int -> LooperM ()
freshLoop i j = do
    fresh <- liftIO Loop.newLoop
    lift $ modify (\s -> s { csLoops = csLoops s Array.// [(APC.Coord (i,j), fresh)] })

clearQueue :: APC.Coord -> LooperM ()
clearQueue coord = do
    lift $ modify (\s -> s { csQueue = filter ((Just coord /=) . fst) (csQueue s) })

tapChannel :: Int -> Int -> Int -> Transition Channel ()
tapChannel i j pos = Transition $ do
    ch <- get
    if | Just (j', Recording) <- chActiveSlot ch,
         j' == j -> do 
            put (ch { chActiveSlot = Just (j, Playing pos) })
            runTransition . transLights $ S.send (APC.InMatrixButton (APC.Coord (i,j)) playingColor)
            runTransition . transActions $ maybeSetQuantization i j
       | Just (j', Playing _) <- chActiveSlot ch,
         j' == j -> do
            runTransition (stopActive i)
       | chSlots ch Array.! j -> do
            runTransition (stopActive i)
            put (ch { chActiveSlot = Just (j, Playing pos) })
            runTransition . transLights $ S.send (APC.InMatrixButton (APC.Coord (i,j)) playingColor)
       -- not (chSlots ch Array.! j)
       | otherwise -> do
            runTransition (stopActive i) 
            put (ch { chSlots = chSlots ch Array.// [(j, True)], chActiveSlot = Just (j, Recording) })
            runTransition . transLights $ S.send (APC.InMatrixButton (APC.Coord (i,j)) recordingColor)
            runTransition . transActions $ freshLoop i j

maybeSetQuantization :: Int -> Int -> LooperM ()
maybeSetQuantization i j = do
    state <- lift get
    M.when (isNothing (csQuantization state)) $ do
        let loop = csLoops state Array.! APC.Coord (i,j)
        loopsize <- liftIO $ Loop.getLoopSize loop
        setQuantization loopsize

setQuantization :: Int -> LooperM ()
setQuantization cycleLength =
    lift $ modify (\s -> s { csQuantization = Just (qlength, csPosition s) })
    where
    -- If we have long loops, half/double until the tempo is reasonable.
    -- This should work for most types of music.
    qlength = until (>= minlength) (* 2) . until (<= maxlength) (`div` 2) $ cycleLength
    maxlength = 44100*4  -- 4 seconds  (60bpm)
    minlength = 44100    -- 1 second  (240bpm)

stopActive :: Int -> Transition Channel ()
stopActive i = Transition $ do
    ch <- get
    if | Just (j, _) <- chActiveSlot ch -> do
            put (ch { chActiveSlot = Nothing })
            runTransition . transLights $ S.send (APC.InMatrixButton (APC.Coord (i,j)) stoppedColor)
       | otherwise  -> return ()

schedule :: (Maybe APC.Coord, Transition ControlState ()) -> LooperM ()
schedule t = lift $ modify (\s -> s { csQueue = t : csQueue s })

activateTransition :: Transition ControlState a -> LooperM a
activateTransition t = do
    state <- lift get
    let ((x,state'), (MonadMonoid lights, MonadMonoid actions)) = runWriter (runStateT (runTransition t) state)
    lift $ put state'
    lights
    actions
    return x

renderMix :: LooperM Mix
renderMix = do
    state <- lift get
    let inputChannel = InputChannel { icLevel = csLevel state, icSends = csSends state }
    let loopChannels = catMaybes . flip map (Array.assocs (csChannels state)) $ \(i,ch) ->
            if | Just (j, chstate) <- chActiveSlot ch
                 , not (chMute ch) -> 
                    Just (MixChannel
                        { mcLoop = csLoops state Array.! APC.Coord (i,j)
                        , mcLevel = csLevel state * chLevel ch
                        , mcState = chstate
                        , mcSends = csSends state
                        })
               | otherwise -> Nothing
    time <- S.getCurrentTime
    return $ Mix time inputChannel (if csBreak state then [] else loopChannels)

runLooper :: Int -> LooperM Mix
runLooper winsize = do
    state <- lift get
    let pos = csPosition state

    runqueue <- case (csBreak state, csQuantization state) of
        (True, _) -> return False
        (False, Nothing) -> return True
        (False, Just (period, phase)) -> do
            let bar = quantPoint period phase pos
            let beat = quantPoint (period `div` 4) phase pos -- TODO more flexible than 4 beats/bar
            -- XXX "24 times per quarter note" but subdiv doesn't match, seems to be 12 times 
            -- per quarter according to the APC.
            let clock = quantPoint (period `div` (2*24)) phase pos
            M.when clock $ S.send APC.InClock
             
            if bar then
                S.send (APC.InMetronome True) >> after (fromMillisec 200) (S.send (APC.InMetronome False))
            else if beat then
                S.send (APC.InMetronome True) >> after (fromMillisec 100) (S.send (APC.InMetronome False))
            else return ()

            return bar
    M.when runqueue $ do
        let queue = csQueue state
        lift $ modify (\s -> s { csQueue = [] })
        forM_ (reverse queue) (activateTransition . snd)

    -- It is important that renderMix reads the *new* state after running the queue.
    mix <- renderMix
    return mix

    where
    quantPoint period phase pos = ((pos-phase) `div` period) /= ((pos-phase) - winsize) `div` period

type IOBuffers = (IOArray.IOUArray Int Double, IOArray.IOUArray Int Double)

data LooperState = LooperState 
    { lsMidiDevs    :: Devs
    , lsStartTime   :: Time
    , lsFrame       :: Int -> LooperM Mix
    , lsSeqState    :: IORef (S.SeqState LooperM ControlIn ControlOut, ControlState)
    , lsBuffers     :: IORef (Int, IOBuffers)
    }

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- openDevs
    let badstate = error "first order of business must be to set state"
    let startlooper = do
            () <- startLooper
            S.getCurrentTime
    ((time0, seqstate), superstate) <- 
        runStateT 
            (S.runSequencerT startlooper =<< S.bootSequencerT devs APC.apc40Raw) 
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
makeBuffers window = (,) <$> buf <*> buf
    where
    buf = IOArray.newArray (0, window-1) 0

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
hsLooperMain looperstate window _inchannels outchannels channels = do
    (seqstate, superstate) <- readIORef (lsSeqState looperstate)
    ((Mix time inputmix channelmixes, seqstate'), superstate') 
        <- runStateT (S.runSequencerT (S.tick window >> lsFrame looperstate window) seqstate) superstate
    writeIORef (lsSeqState looperstate) (seqstate', superstate')
    let position = toSamples (time `diff` lsStartTime looperstate)

    inbuf <- peekElemOff channels 0
    (inbufarray, outbufarray) <- getBuffers looperstate window
    forM_ [0..window-1] $ \i -> IOArray.writeArray inbufarray i =<< realToFrac <$> peekElemOff inbuf i
    forM_ [0..window-1] $ \i -> IOArray.writeArray outbufarray i 0

    forM_ channelmixes $ \MixChannel{..} -> 
        case mcState of
            Recording -> Loop.append mcLoop position inbufarray
            Playing phase -> Loop.play mcLoop (position - phase) mcLevel outbufarray

    mainoutbuf <- peekElemOff channels 0
    sendbufs <- mapM (peekElemOff channels) [1..min (outchannels-1) 8]
    
    forM_ [0..window-1] $ \i -> do
        sample <- realToFrac <$> IOArray.readArray outbufarray i
        pokeElemOff mainoutbuf i sample
        forM_ (zip [1..8] sendbufs) $ \(sendix, sendbuf) -> 
            pokeElemOff sendbuf i ((icSends inputmix Array.! sendix) * sample)

hs_looper_uilog :: StablePtr LooperState -> IO C.CString
hs_looper_uilog state = wrapErrors "hs_looper_uilog" $ do
    looperstate <- deRefStablePtr state
    C.newCString =<< hsLooperUILog looperstate  -- MUST BE free()d BY CLIENT

hsLooperUILog :: LooperState -> IO String
hsLooperUILog lstate = do 
    state <- snd <$> readIORef (lsSeqState lstate)
    maybelines <- forM (Array.assocs (csChannels state)) $ \(i,ch) ->
        if | Just (j, chstate) <- chActiveSlot ch -> do
                loopsize <- show <$> Loop.getLoopSize (csLoops state Array.! APC.Coord (i,j))
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
    time <- Clock.getCurrentTime
    IO.withFile "/tmp/looperlog" IO.AppendMode $ \fh -> do
        IO.hPutStrLn fh $ show time ++ " : " ++ entry ++ " : " ++ show e
    Exc.throwIO e

(>>) :: (Monad m) => m () -> m a -> m a
a >> b = a >>= \() -> b
