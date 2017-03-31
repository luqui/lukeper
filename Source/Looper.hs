{-# LANGUAGE FlexibleContexts, RecordWildCards, ScopedTypeVariables #-}

module Looper where

import qualified Control.Exception as Exc
import qualified Data.Array.IO as Array
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified System.IO as IO

import Control.Monad (forM_)

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


data LooperState = LooperState 
    { lsMidiDevs    :: APC.Devs
    , lsLoop        :: Loop.Loop
    , lsController  :: APC.Control APC.MIDIIO (APC.Coord, APC.RGBColorState) (APC.Coord, Bool)
    }

hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = wrapErrors "hs_looper_init" $ do
    devs <- APC.openDevs
    loop <- Loop.newLoop
    controller <- APC.runMIDIIO APC.rgbMatrix (snd devs)
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsLoop = loop
                               , lsController = controller
                               }

hs_looper_main :: StablePtr LooperState -> Word32 -> Word32 -> Word32 -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hs_looper_main state window input output channels = wrapErrors "hs_looper_main" $ do
    looperstate <- deRefStablePtr state
    hsLooperMain looperstate (fromIntegral window) (fromIntegral input) (fromIntegral output) channels

hsLooperMain :: LooperState -> Int -> Int -> Int -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hsLooperMain looperstate window _inchannels _outchannels channels = do
    events <- APC.recvC (lsMidiDevs looperstate) (lsController looperstate)

    Loop.modifyLoopState (lsLoop looperstate) $ \state ->
         foldr (\e s -> 
                 if e == (APC.Coord (1,1), True)
                   then case s of
                          Loop.Disabled  -> Loop.Appending
                          Loop.Appending -> Loop.Playing
                          Loop.Playing   -> Loop.Playing
                   else s
                ) state events


    inbuf <- peekElemOff channels 0
    -- TODO, don't allocate every time
    inbufarray <- Array.newListArray (0,window-1) 
                    =<< mapM (fmap realToFrac . peekElemOff inbuf) [0..window-1]
    outbufarray <- Array.newArray (0,window-1) 0

    Loop.runLoop (lsLoop looperstate) inbufarray outbufarray

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
