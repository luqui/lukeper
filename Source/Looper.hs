{-# LANGUAGE FlexibleContexts, MultiWayIf, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Looper where

import qualified Control.Exception as Exc
import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO

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

type ControlIn = (APC.Coord, LongPress)
type ControlOut = (APC.Coord, APC.RGBColorState)
type LooperM = S.SequencerT ControlIn ControlOut (StateT ControlState IO)

type Mix = [(Loop.Loop, Double, ActiveSlotState, Int)]


data ControlState = ControlState
    { csChannels :: Array.Array Int Channel
    , csPosition :: Int
    , csQuantization :: Maybe (Int, Int)   -- period, phase  (i.e. (period*n + phase) is a barline)
    , csLoops :: Array.Array APC.Coord Loop.Loop
    , csQueue :: [Transition ControlState]  -- reversed
    }

data ActiveSlotState = Recording | Playing Int -- int is phase
    deriving (Eq, Show)

data Channel = Channel
    { chSlots :: Array.Array Int Bool
    , chActiveSlot :: Maybe (Int, ActiveSlotState)
    }


data Transition s = Transition { runTransition :: s -> (s, LooperM (), LooperM ()) }

instance Monoid (Transition s) where
    mempty = Transition (\t -> (t, return (), return ()))
    mappend (Transition f) (Transition g) = Transition $ \t -> 
        let (t', c, a) = f t
            (t'', c', a') = g t'
        in (t'', c >> c', a >> a')


transChannel :: Int -> Transition Channel -> Transition ControlState
transChannel i (Transition f) = Transition $ \s -> 
        let (ch', lights, changes) = f (csChannels s Array.! i) in
        (s { csChannels = csChannels s Array.// [(i, ch')] }, lights, changes)

query :: (s -> Transition s) -> Transition s
query f = Transition (\s -> runTransition (f s) s)

startLooper :: LooperM (Int -> LooperM Mix)
startLooper = do
    -- setup state
    lift $ do
        loops <- Array.array (APC.Coord (1,1), APC.Coord (8,5)) <$> 
                 sequence [ (APC.Coord (i,j),) <$> liftIO Loop.newLoop | i <- [1..8], j <- [1..5] ]
        put $ ControlState
            { csChannels = Array.listArray (1,8) . replicate 8 $
                Channel { chSlots = Array.listArray (1,5) . replicate 5 $ False
                        , chActiveSlot = Nothing
                        }
            , csPosition = 0
            , csQuantization = Nothing
            , csLoops = loops
            , csQueue = []
            } 

    S.whenever (\e -> snd e == PressDown) $ \(APC.Coord (i,j), _) -> do
        schedule $ query (\s -> transChannel i (tapChannel i j (csPosition s)))
    S.whenever (\e -> snd e == PressLong) $ \(APC.Coord (i,j), _) ->
        activateTransition $ transChannel i . (stopActive i <>) . Transition $ \ch -> 
            (ch { chSlots = chSlots ch Array.// [(j, False)] },
             S.send (APC.Coord (i,j), offColor),
             liftIO . Loop.clearLoop . (Array.! APC.Coord (i,j)) =<< lift (gets csLoops))

    return $ \winsize -> do
        state <- lift get
        let pos = csPosition state

        let runqueue = case csQuantization state of
                        Nothing -> True
                        Just (period, phase) -> 
                            ((pos-phase) `div` period) /= ((pos-phase)-winsize) `div` period
        if runqueue then do
            let queue = csQueue state
            lift $ modify (\s -> s { csQueue = [] })
            forM_ (reverse queue) activateTransition
        else return ()

        let mix = catMaybes . flip map (Array.assocs (csChannels state)) $ \(i,ch) ->
                if | Just (j, chstate) <- chActiveSlot ch -> 
                        Just (csLoops state Array.! APC.Coord (i,j), 1, chstate, pos)
                   | otherwise -> Nothing
        lift $ modify (\s -> s { csPosition = csPosition s + winsize })
        return mix

    where
    tapChannel i j pos = query $ \ch -> 
        if | Just (j', Recording) <- chActiveSlot ch,
             j' == j -> Transition $ const (ch { chActiveSlot = Just (j, Playing pos) }, 
                                            S.send (APC.Coord (i,j), playingColor),
                                            maybeSetQuantization i j)
           | Just (j', Playing _) <- chActiveSlot ch,
             j' == j -> stopActive i
           | chSlots ch Array.! j -> stopActive i <> Transition (const
                     (ch { chActiveSlot = Just (j, Playing pos) },
                      S.send (APC.Coord (i,j), playingColor),
                      return ()))
           -- not (chSlots ch Array.! j)
           | otherwise -> stopActive i <> Transition (const
                     (ch { chSlots = chSlots ch Array.// [(j, True)], chActiveSlot = Just (j, Recording) },
                      S.send (APC.Coord (i,j), recordingColor),
                      return ()))

    maybeSetQuantization i j = do
        state <- lift get
        if isNothing (csQuantization state) then do
            let loop = csLoops state Array.! APC.Coord (i,j)
            loopsize <- liftIO $ Loop.getLoopSize loop
            lift $ modify (\s -> s { csQuantization = Just (loopsize, csPosition state) })
        else return ()
            
    stopActive i = Transition $ \ch ->
        if | Just (j, _) <- chActiveSlot ch -> 
                (ch { chActiveSlot = Nothing }, S.send (APC.Coord (i,j), stoppedColor), return ())
           | otherwise  -> (ch, return (), return ())

    playingColor   = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)
    recordingColor = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7)
    stoppedColor   = APC.RGBSolid (APC.velToRGBColor 14)
    offColor       = APC.RGBOff

    schedule t = lift $ modify (\s -> s { csQueue = t : csQueue s })

    activateTransition t = do
        (state', lights, action) <- runTransition t <$> lift get
        lift $ put state'
        lights
        action

data LooperState = LooperState 
    { lsMidiDevs    :: Devs
    , lsFrame       :: Int -> LooperM Mix
    , lsSeqState    :: IORef (S.SeqState LooperM ControlIn ControlOut, ControlState)
    }

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- openDevs
    ((frame, seqstate), superstate) <- runStateT (S.runSequencerT startLooper =<< S.bootSequencerT devs APC.apc40Raw) (error "first order of business must be to set state")
    seqstateref <- newIORef (seqstate, superstate)
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsFrame = frame
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
    return . unlines . catMaybes $ maybelines


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
