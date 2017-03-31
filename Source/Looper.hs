{-# LANGUAGE FlexibleContexts, RecordWildCards, ScopedTypeVariables #-}

module Looper where

import qualified Control.Exception as Exc
import qualified Data.Array.IO as Array
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO

import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (liftIO)

import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

import qualified APC40mkII as APC
import qualified Loop as Loop
import qualified Sequencer as S


foreign export ccall hs_looper_init :: IO (StablePtr LooperState)
foreign export ccall hs_looper_main 
    :: StablePtr LooperState
    -> Word32                          -- window size
    -> Word32                          -- input channels
    -> Word32                          -- output channels
    -> Foreign.Ptr (Foreign.Ptr Float) -- channel data
    -> IO ()
foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()

type LooperM = S.SequencerT (APC.Coord, Bool) (APC.Coord, APC.RGBColorState) APC.MIDIIO

startLooper :: [Loop.Loop] -> LooperM ()
startLooper loops = sequence_ [ launchButton loop (APC.Coord (i,1)) | (i, loop) <- zip [1..] loops ]
    where
    launchButton :: Loop.Loop -> APC.Coord -> LooperM ()
    launchButton loop coord = S.when (\e -> e == (coord, True)) $ do
        S.send (coord, APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 6) (APC.velToRGBColor 7))
        liftIO $ Loop.setLoopState loop Loop.Appending
        playStop loop coord

    playStop :: Loop.Loop -> APC.Coord -> LooperM ()
    playStop loop coord = S.when (\e -> e == (coord, True)) $ do
        S.send (coord, APC.RGBPulsing APC.Subdiv4 (APC.velToRGBColor 22) (APC.velToRGBColor 23)) 
        liftIO $ Loop.setLoopState loop Loop.Playing
        S.when (\e -> e == (coord, True)) $ do
            S.send (coord, APC.RGBSolid (APC.velToRGBColor 14))
            liftIO $ Loop.setLoopState loop Loop.Disabled
            playStop loop coord

data LooperState = LooperState 
    { lsMidiDevs    :: APC.Devs
    , lsLoops       :: [Loop.Loop]
    , lsSeqState    :: IORef (S.SeqState LooperM (APC.Coord, Bool) (APC.Coord, APC.RGBColorState))
    }

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- APC.openDevs
    loops <- replicateM 8 Loop.newLoop
    controller <- APC.runMIDIIO APC.rgbMatrix (snd devs)
    (_, seqstate) <- APC.runMIDIIO 
        (S.runSequencerT (startLooper loops) (S.newSeqState devs controller)) (snd devs)
    seqstateref <- newIORef seqstate
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsLoops = loops
                               , lsSeqState = seqstateref
                               }

hs_looper_main :: StablePtr LooperState -> Word32 -> Word32 -> Word32 -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hs_looper_main state window input output channels = wrapErrors "hs_looper_main" $ do
    looperstate <- deRefStablePtr state
    hsLooperMain looperstate (fromIntegral window) (fromIntegral input) (fromIntegral output) channels

hsLooperMain :: LooperState -> Int -> Int -> Int -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hsLooperMain looperstate window _inchannels _outchannels channels = do
    seqstate <- readIORef (lsSeqState looperstate)
    (_, seqstate') <- APC.runMIDIIO (S.runSequencerT S.tick seqstate) (snd (lsMidiDevs looperstate))
    writeIORef (lsSeqState looperstate) seqstate'
    
    inbuf <- peekElemOff channels 0
    -- TODO, don't allocate every time
    inbufarray <- Array.newListArray (0,window-1) 
                    =<< mapM (fmap realToFrac . peekElemOff inbuf) [0..window-1]
    outbufarray <- Array.newArray (0,window-1) 0

    mapM_ (\loop -> Loop.runLoop loop inbufarray outbufarray) (lsLoops looperstate)

    outbufL <- peekElemOff channels 0
    outbufR <- peekElemOff channels 1
    forM_ [0..window-1] $ \i -> do
        sample <- realToFrac <$> Array.readArray outbufarray i
        pokeElemOff outbufL i sample
        pokeElemOff outbufR i sample

hs_looper_exit :: StablePtr LooperState -> IO ()
hs_looper_exit state = wrapErrors "hs_looper_exit" $ do
    looperstate <- deRefStablePtr state
    APC.closeDevs (lsMidiDevs looperstate)
    freeStablePtr state


wrapErrors :: String -> IO a -> IO a
wrapErrors entry action = Exc.catch action $ \(e :: Exc.SomeException) -> do
    time <- Clock.getCurrentTime
    IO.withFile "/tmp/looperlog" IO.AppendMode $ \fh -> do
        IO.hPutStrLn fh $ show time ++ " : " ++ entry ++ " : " ++ show e
    Exc.throwIO e
