{-# LANGUAGE FlexibleContexts, MultiWayIf, NondecreasingIndentation, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Looper where

import Prelude hiding (break)

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
import Control.Monad.Trans.State (StateT, get, gets, put, modify, runStateT)
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid (Monoid(..), (<>))

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

type Mix = [(Loop.Loop, Double, ActiveSlotState, Int)]


data ControlState = ControlState
    { csChannels :: Array.Array Int Channel
    , csPosition :: Int
    , csQuantization :: Maybe (Int, Int)   -- period, phase  (i.e. (period*n + phase) is a barline)
    , csLoops :: Array.Array APC.Coord Loop.Loop
    , csQueue :: [(Maybe APC.Coord, Transition ControlState)]  -- reversed
    , csLevel :: Double
    , csBreak :: Bool
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


data Transition s = Transition { runTransition :: s -> (s, LooperM (), LooperM ()) }

instance Monoid (Transition s) where
    mempty = Transition (\t -> (t, return (), return ()))
    mappend (Transition f) (Transition g) = Transition $ \t -> 
        let (t', c, a) = f t
            (t'', c', a') = g t'
        in (t'', c >> c', a >> a')

playingColor,recordingColor,stoppedColor,offColor :: APC.RGBColorState 
playingColor   = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)
recordingColor = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7)
stoppedColor   = APC.RGBSolid (APC.velToRGBColor 14)
offColor       = APC.RGBOff

transChannel :: Int -> Transition Channel -> Transition ControlState
transChannel i (Transition f) = Transition $ \s -> 
        let (ch', lights, changes) = f (csChannels s Array.! i) in
        (s { csChannels = csChannels s Array.// [(i, ch')] }, lights, changes)

transAllChannels :: Transition Channel -> Transition ControlState
transAllChannels t = query $ \s ->
    mconcat [ transChannel i t | i <- Array.indices (csChannels s) ]

restartChannel :: Int -> Transition Channel
restartChannel phase = Transition $ \ch -> 
    (ch { chActiveSlot = fmap (\(n, slotstate) ->
            (n, case slotstate of
                    Playing _ -> Playing phase
                    Recording -> Recording)) (chActiveSlot ch) },
     return (),
     return ())

query :: (s -> Transition s) -> Transition s
query f = Transition (\s -> runTransition (f s) s)

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
            } 

    -- TODO, I think we can use a single MonadPlus for this, e.g.
    --    APC.MatrixButton coord APC.PressDown <- e
    --    ...
    -- and mzero means "doesn't fire"
    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressDown <- return e
        lift . schedule . (Just (APC.Coord (i,j)),) $ query (\s -> transChannel i (tapChannel i j (csPosition s)))
    S.whenever $ \e -> do
        APC.OutMatrixButton (APC.Coord (i,j)) PressLong <- return e
        lift . activateTransition $ transChannel i . (stopActive i <>) . Transition $ \ch -> 
            (ch { chSlots = chSlots ch Array.// [(j, False)] },
             S.send (APC.InMatrixButton (APC.Coord (i,j)) offColor),
             do freshLoop i j
                clearQueue (APC.Coord (i,j)))
    S.whenever $ \e -> do
        APC.OutFader i level <- return e
        lift $
            if | 1 <= i && i <= 8 -> 
                activateTransition . transChannel i . Transition $ \ch -> 
                    (ch { chLevel = level }, return (), return ())
               | i == 9 ->
                activateTransition . Transition $ \cs -> (cs { csLevel = level }, return (), return ())
               | otherwise ->
                return ()
    S.whenever $ \e -> do
        APC.OutTempoChange dt <- return e
        lift $ do
            pos <- lift $ gets csPosition
            activateTransition . Transition $ \s ->
                let stretch = 1.01^^(-dt) in
                (s { csLoops = fmap (Loop.stretch stretch) (csLoops s)
                   , csQuantization = fmap (\(period, phase) -> 
                        (round (fromIntegral period * stretch), stretchPhase stretch pos phase)) (csQuantization s)
                   , csChannels = fmap (\ch -> ch { 
                        chActiveSlot = (fmap.second) (stretchActiveSlot stretch pos) (chActiveSlot ch) }) (csChannels s)
                   }
                , return (), return ())
    S.whenever $ \e -> do
        APC.OutUnmuteButton ch True <- return e
        lift . activateTransition . transChannel ch . Transition $ \s ->
            (s { chMute = not (chMute s) },
             S.send (APC.InUnmuteButton ch (chMute s)),
             return ())
    S.whenever $ \e -> do
        APC.OutStopAllButton True <- return e
        lift $ do
            quant <- lift $ gets csQuantization
            break <- lift $ gets csBreak
            let delay | Just (bar, _) <- quant, not break = (1000*bar) `div` (16*44100)  -- wait 1 sixteenth to break for a final hit
                      | otherwise                         = 0
            after (fromIntegral delay) . activateTransition . query $ \state ->
                if not (csBreak state) then
                    Transition (\s -> (s { csBreak = True }, return (), return ()))
                else
                    transAllChannels (restartChannel (csPosition state)) <> 
                    Transition (\s -> (s { csBreak = False
                                         , csQuantization = fmap (\(l,_) -> (l, csPosition s)) (csQuantization s) }
                                      , return ()
                                      , return ()))
    S.when $ \e -> do 
        APC.OutSessionButton True <- return e
        -- XXX hack -- we can't reboot in the middle of a conditional event handler,
        -- because of the way Sequencer.processEvents works. It will not correctly
        -- clear conditional events.  So we use S.after 0 to convert into a timed
        -- event, which works correctly.
        lift . after 0 $ do
            S.rebootSequencerT APC.apc40Raw
            startLooper

freshLoop :: Int -> Int -> LooperM ()
freshLoop i j = do
    fresh <- liftIO Loop.newLoop
    lift $ modify (\s -> s { csLoops = csLoops s Array.// [(APC.Coord (i,j), fresh)] })

clearQueue :: APC.Coord -> LooperM ()
clearQueue coord = do
    lift $ modify (\s -> s { csQueue = filter ((Just coord /=) . fst) (csQueue s) })

tapChannel :: Int -> Int -> Int -> Transition Channel
tapChannel i j pos = query $ \ch -> 
    if | Just (j', Recording) <- chActiveSlot ch,
         j' == j -> Transition $ const (ch { chActiveSlot = Just (j, Playing pos) }, 
                                        S.send (APC.InMatrixButton (APC.Coord (i,j)) playingColor),
                                        do maybeSetQuantization i j)
       | Just (j', Playing _) <- chActiveSlot ch,
         j' == j -> stopActive i
       | chSlots ch Array.! j -> stopActive i <> Transition (const
                 (ch { chActiveSlot = Just (j, Playing pos) },
                  S.send (APC.InMatrixButton (APC.Coord (i,j)) playingColor),
                  return ()))
       -- not (chSlots ch Array.! j)
       | otherwise -> stopActive i <> Transition (const
                 (ch { chSlots = chSlots ch Array.// [(j, True)], chActiveSlot = Just (j, Recording) },
                  S.send (APC.InMatrixButton (APC.Coord (i,j)) recordingColor),
                  freshLoop i j))

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

stopActive :: Int -> Transition Channel
stopActive i = Transition $ \ch ->
    if | Just (j, _) <- chActiveSlot ch -> 
            (ch { chActiveSlot = Nothing }, S.send (APC.InMatrixButton (APC.Coord (i,j)) stoppedColor), return ())
       | otherwise  -> (ch, return (), return ())

schedule :: (Maybe APC.Coord, Transition ControlState) -> LooperM ()
schedule t = lift $ modify (\s -> s { csQueue = t : csQueue s })

activateTransition :: Transition ControlState -> LooperM ()
activateTransition t = do
    (state', lights, action) <- runTransition t <$> lift get
    lift $ put state'
    lights
    action

renderMix :: LooperM Mix
renderMix = do
    state <- lift get
    if csBreak state then return [] else do
    return $ catMaybes . flip map (Array.assocs (csChannels state)) $ \(i,ch) ->
            if | Just (j, chstate) <- chActiveSlot ch
                 , not (chMute ch) -> 
                    Just (csLoops state Array.! APC.Coord (i,j), csLevel state * chLevel ch, chstate, csPosition state)
               | otherwise -> Nothing

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
                S.send (APC.InMetronome True) >> after 200 (S.send (APC.InMetronome False))
            else if beat then
                S.send (APC.InMetronome True) >> after 100 (S.send (APC.InMetronome False))
            else return ()

            return bar
    M.when runqueue $ do
        let queue = csQueue state
        lift $ modify (\s -> s { csQueue = [] })
        forM_ (reverse queue) (activateTransition . snd)

    -- It is important that renderMix reads the *new* state after running the queue.
    mix <- renderMix
    lift $ modify (\s -> s { csPosition = csPosition s + winsize })
    return mix

    where
    quantPoint period phase pos = ((pos-phase) `div` period) /= ((pos-phase) - winsize) `div` period


data LooperState = LooperState 
    { lsMidiDevs    :: Devs
    , lsFrame       :: Int -> LooperM Mix
    , lsSeqState    :: IORef (S.SeqState LooperM ControlIn ControlOut, ControlState)
    }

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- openDevs
    (((), seqstate), superstate) <- runStateT (S.runSequencerT startLooper =<< S.bootSequencerT devs APC.apc40Raw) (error "first order of business must be to set state")
    seqstateref <- newIORef (seqstate, superstate)
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsFrame = runLooper
                               , lsSeqState = seqstateref
                               }

hs_looper_main :: StablePtr LooperState -> Word32 -> Word32 -> Word32 -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hs_looper_main state window input output channels = wrapErrors "hs_looper_main" $ do
    looperstate <- deRefStablePtr state
    hsLooperMain looperstate (fromIntegral window) (fromIntegral input) (fromIntegral output) channels

hsLooperMain :: LooperState -> Int -> Int -> Int -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hsLooperMain looperstate window _inchannels _outchannels channels = do
    (seqstate, superstate) <- readIORef (lsSeqState looperstate)
    ((mix, seqstate'), superstate') <- runStateT (S.runSequencerT (S.tick >> lsFrame looperstate window) seqstate) superstate
    writeIORef (lsSeqState looperstate) (seqstate', superstate')
    
    inbuf <- peekElemOff channels 0
    -- TODO, don't allocate every time
    inbufarray <- IOArray.newListArray (0,window-1) 
                    =<< mapM (fmap realToFrac . peekElemOff inbuf) [0..window-1]
    outbufarray <- IOArray.newArray (0,window-1) 0

    forM_ mix $ \(loop, level, state, pos) -> 
        case state of
            Recording -> Loop.append loop pos inbufarray
            Playing phase -> Loop.play loop (pos - phase) level outbufarray

    outbufL <- peekElemOff channels 0
    outbufR <- peekElemOff channels 1
    forM_ [0..window-1] $ \i -> do
        sample <- realToFrac <$> IOArray.readArray outbufarray i
        pokeElemOff outbufL i sample
        pokeElemOff outbufR i sample

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
