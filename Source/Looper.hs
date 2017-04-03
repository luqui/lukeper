{-# LANGUAGE FlexibleContexts, MultiWayIf, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Looper where

import qualified Control.Exception as Exc
import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList)
import Data.Monoid (Monoid(..), (<>))

import Control.Monad.Trans.State (StateT, gets, put, modify, runStateT)
import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

import qualified APC40mkII_Raw as APC
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
foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()

type ControlIn = (APC.Coord, LongPress)
type ControlOut = (APC.Coord, APC.RGBColorState)
type LooperM = S.SequencerT ControlIn ControlOut (StateT SuperState IO)

type Mix = [(Double, Loop.Loop)]


data TransState = TransState
    { tsChannels :: Array.Array Int Channel
    }

data ActiveSlotState = Recording | Playing
    deriving (Eq)

data Channel = Channel
    { chSlots :: Array.Array Int Bool
    , chActiveSlot :: Maybe (Int, ActiveSlotState)
    }


data Transition s = Transition { runTransition :: s -> (s, LooperM ()) }

instance Monoid (Transition s) where
    mempty = Transition (\t -> (t, return ()))
    mappend (Transition f) (Transition g) = Transition $ \t -> 
        let (t', m) = f t
            (t'', m') = g t'
        in (t'', m >> m')


data SuperState = SuperState 
    { ssTransState :: TransState
    , ssLoops :: Array.Array APC.Coord Loop.Loop
    }

transChannel :: Int -> Transition Channel -> Transition TransState
transChannel i (Transition f) = Transition $ \s -> 
        let (ch', lights) = f (tsChannels s Array.! i) in
        (s { tsChannels = tsChannels s Array.// [(i, ch')] }, lights)

query :: (s -> Transition s) -> Transition s
query f = Transition (\s -> runTransition (f s) s)

startLooper :: LooperM (LooperM Mix)
startLooper = do
    -- setup state
    lift $ do
        loops <- Array.array (APC.Coord (1,1), APC.Coord (8,5)) <$> 
                 sequence [ (APC.Coord (i,j),) <$> liftIO Loop.newLoop | i <- [1..8], j <- [1..5] ]
        put $ SuperState
            { ssTransState = TransState
                { tsChannels = Array.listArray (1,8) . replicate 8 $
                    Channel { chSlots = Array.listArray (1,5) . replicate 5 $ False
                            , chActiveSlot = Nothing
                            }
                }
            , ssLoops = loops
            } 

    S.whenever (\e -> snd e == PressDown) $ \(APC.Coord (i,j), _) ->
        schedule . transChannel i $ tapChannel i j
    S.whenever (\e -> snd e == PressLong) $ \(APC.Coord (i,j), _) ->
        schedule . transChannel i . (stopActive i <>) . Transition $ \ch -> 
            (ch { chSlots = chSlots ch Array.// [(j, False)] },
             S.send (APC.Coord (i,j), offColor))

    return $ do
        loops <- lift $ gets ssLoops
        activeLoopIds <- lift $ map chActiveSlot <$> gets (toList . tsChannels . ssTransState)
        activeLoops <- liftIO . sequence $ do
            (i, Just (j, state)) <- zip [1..] activeLoopIds
            let loop = loops Array.! APC.Coord (i,j)
            let setAction = case state of
                    Recording -> do
                        loopstate <- Loop.getLoopState loop
                        if loopstate /= Loop.Appending then Loop.clearLoop loop else return ()
                        Loop.setLoopState loop Loop.Appending
                    Playing -> Loop.setLoopState loop Loop.Playing
            return $ setAction >> return loop
        return $ map (1,) activeLoops

    where
    tapChannel i j = query $ \ch -> 
        if | Just (j', Recording) <- chActiveSlot ch,
             j' == j -> Transition $ const (ch { chActiveSlot = Just (j, Playing) }, 
                                            S.send (APC.Coord (i,j), playingColor))
           | Just (j', Playing) <- chActiveSlot ch,
             j' == j -> stopActive i
           | chSlots ch Array.! j -> stopActive i <> Transition (const
                     (ch { chActiveSlot = Just (j, Playing) },
                      S.send (APC.Coord (i,j), playingColor)))
           -- not (chSlots ch Array.! j)
           | otherwise -> stopActive i <> Transition (const
                     (ch { chSlots = chSlots ch Array.// [(j, True)], chActiveSlot = Just (j, Recording) },
                      S.send (APC.Coord (i,j), recordingColor)))

    stopActive i = Transition $ \ch ->
        if | Just (j, _) <- chActiveSlot ch -> 
                (ch { chActiveSlot = Nothing }, S.send (APC.Coord (i,j), stoppedColor))
           | otherwise  -> (ch, return ())

    playingColor   = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)
    recordingColor = APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7)
    stoppedColor   = APC.RGBSolid (APC.velToRGBColor 14)
    offColor       = APC.RGBOff

    schedule t = do
        transstate <- lift $ gets ssTransState
        let (transstate', lights) = runTransition t transstate
        lift $ modify (\s -> s { ssTransState = transstate' })
        lights


data LooperState = LooperState 
    { lsMidiDevs    :: Devs
    , lsFrame       :: LooperM Mix
    , lsSeqState    :: IORef (S.SeqState LooperM ControlIn ControlOut, SuperState)
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
    ((mix, seqstate'), superstate') <- runStateT (S.runSequencerT (S.tick >> lsFrame looperstate) seqstate) superstate
    writeIORef (lsSeqState looperstate) (seqstate', superstate')
    
    inbuf <- peekElemOff channels 0
    -- TODO, don't allocate every time
    inbufarray <- IOArray.newListArray (0,window-1) 
                    =<< mapM (fmap realToFrac . peekElemOff inbuf) [0..window-1]
    outbufarray <- IOArray.newArray (0,window-1) 0

    forM_ mix $ \(level,loop) -> 
        Loop.runLoop loop inbufarray level outbufarray

    outbufL <- peekElemOff channels 0
    outbufR <- peekElemOff channels 1
    forM_ [0..window-1] $ \i -> do
        sample <- realToFrac <$> IOArray.readArray outbufarray i
        pokeElemOff outbufL i sample
        pokeElemOff outbufR i sample

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
