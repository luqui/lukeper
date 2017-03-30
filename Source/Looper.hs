{-# LANGUAGE RecordWildCards #-}

module Looper where

import qualified Data.Array.IO as Array
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified System.MIDI as MIDI

import Control.Monad (filterM, (<=<), forM_)

import Control.Arrow (second)
import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

import qualified APC40mkII as APC

data LoopBuffer = LoopBuffer
    { loopSize :: Int
    , loopPos  :: Int
    , loopData :: Array.IOArray Int Double
    }

getSize :: Array.IOArray Int a -> IO Int
getSize a = do
    (0, bufEnd) <- Array.getBounds a
    return $ bufEnd+1

newLoopBuffer :: IO (IORef LoopBuffer)
newLoopBuffer = do
    loopdata <- Array.newArray (0,44099) 0
    newIORef $ LoopBuffer { loopSize = 0, loopPos = 0, loopData = loopdata }

record :: IORef LoopBuffer -> Array.IOArray Int Double -> IO ()
record loopbufref inbuf = do
    loopbuf <- readIORef loopbufref
    bufsize <- getSize inbuf
    loopbufsize <- getSize (loopData loopbuf)
    -- NB. reallocates at most once.  *Should* be enough but not technically correct.
    loopdata' <- if loopSize loopbuf + bufsize > loopbufsize
                    then Array.newListArray (0, loopbufsize*2-1) =<< Array.getElems (loopData loopbuf)
                    else return (loopData loopbuf)
    forM_ [0..bufsize-1] $ \i -> do
        Array.writeArray loopdata' (loopSize loopbuf + i) =<< Array.readArray inbuf i
    writeIORef loopbufref $  
        LoopBuffer { loopSize = loopSize loopbuf + bufsize
                   , loopPos  = 0
                   , loopData = loopdata'
                   }

play :: IORef LoopBuffer -> Array.IOArray Int Double -> IO ()
play loopbufref outbuf = do
    loopbuf <- readIORef loopbufref
    bufsize <- getSize outbuf
    forM_ [0..bufsize-1] $ \i -> do
        cur <- Array.readArray outbuf i
        Array.writeArray outbuf i . (+cur) =<< Array.readArray (loopData loopbuf) ((loopPos loopbuf + i) `mod` loopSize loopbuf)
    writeIORef loopbufref $ loopbuf { loopPos = loopPos loopbuf + bufsize `mod` loopSize loopbuf }


data RecordState = NotYet | Recording | Done
    deriving (Eq, Show)

data LooperState = LooperState 
    { lsMidiDevs    :: APC.Devs
    , lsLoop        :: IORef LoopBuffer
    , lsRecordState :: IORef RecordState
    , lsController  :: APC.Control APC.MIDIIO (APC.Coord, APC.RGBColorState) (APC.Coord, Bool)
    }

foreign export ccall hs_looper_init :: IO (StablePtr LooperState)
hs_looper_init = do
    devs <- APC.openDevs
    loopbuf <- newLoopBuffer
    recordstate <- newIORef NotYet
    controller <- APC.runMIDIIO APC.rgbMatrix (snd devs)
    newStablePtr $ LooperState { lsMidiDevs = devs
                               , lsLoop = loopbuf
                               , lsRecordState = recordstate
                               , lsController = controller
                               }

foreign export ccall hs_looper_main 
    :: StablePtr LooperState
    -> Word32                          -- window size
    -> Word32                          -- input channels
    -> Word32                          -- output channels
    -> Foreign.Ptr (Foreign.Ptr Float) -- channel data
    -> IO ()
hs_looper_main state window input output channels = do
    looperstate <- deRefStablePtr state
    hsLooperMain looperstate (fromIntegral window) (fromIntegral input) (fromIntegral output) channels

hsLooperMain :: LooperState -> Int -> Int -> Int -> Foreign.Ptr (Foreign.Ptr Float) -> IO ()
hsLooperMain looperstate window inchannels outchannels channels = do
    events <- APC.recvC (lsMidiDevs looperstate) (lsController looperstate)
    recstate <- readIORef (lsRecordState looperstate)
    let recstate' = foldr (\e s -> 
                        if e == (APC.Coord (1,1), True)
                          then case s of
                                 NotYet -> Recording
                                 Recording -> Done
                                 Done -> Done
                          else s
                       ) recstate events

    inbuf <- peekElemOff channels 0
    -- TODO, don't allocate every time
    inbufarray <- Array.newListArray (0,window-1) 
                    =<< mapM (fmap realToFrac . peekElemOff inbuf) [0..window-1]
    outbufarray <- Array.newArray (0,window-1) 0

    case recstate' of
        NotYet -> return ()
        Recording -> record (lsLoop looperstate) inbufarray
        Done -> play (lsLoop looperstate) outbufarray
    writeIORef (lsRecordState looperstate) recstate'

    outbufL <- peekElemOff channels 0
    outbufR <- peekElemOff channels 1
    forM_ [0..window-1] $ \i -> do
        sample <- realToFrac <$> Array.readArray outbufarray i
        pokeElemOff outbufL i sample
        pokeElemOff outbufR i sample

foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()
hs_looper_exit state = do
    looperstate <- deRefStablePtr state
    APC.closeDevs (lsMidiDevs looperstate)
    freeStablePtr state
