{-# LANGUAGE RecordWildCards #-}

module Looper where

import qualified Data.Array.IO as Array
import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified System.MIDI as MIDI

import Control.Monad (filterM, (<=<), forM_)

import Data.IORef
import Data.Word
import Foreign.StablePtr
import Foreign.Storable

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
    { lsMidiSource :: MIDI.Connection
    , lsMidiDest   :: MIDI.Connection
    , lsLoop       :: Array.IOUArray Int Float
    , lsIndex      :: IORef Int
    }

findConnection :: (MIDI.MIDIHasName a) => String -> [a] -> IO a
findConnection name = fmap head . filterM (\c -> (name ==) <$> MIDI.getName c)
    where
    getConn (x:_) = x
    getConn [] = error $ "No MIDI device with name " ++ show name

hs_looper_init = do
    source <- flip MIDI.openSource Nothing =<< findConnection "APC40 mkII" =<< MIDI.enumerateSources
    dest <- MIDI.openDestination =<< findConnection "APC40 mkII" =<< MIDI.enumerateDestinations
    array <- Array.newArray (0,44099) 0
    index <- newIORef 0
    MIDI.start source
    newStablePtr $ LooperState source dest array index
hs_looper_main state window input output channels = do
    LooperState src dest loop ixref <- deRefStablePtr state
    time <- floor . realToFrac . Clock.utctDayTime <$> Clock.getCurrentTime
    MIDI.send dest $ MIDI.MidiMessage 0 (MIDI.NoteOn 32 (fromIntegral (time `mod` 128)))

    loopix0 <- readIORef ixref
    forM_ [0..fromIntegral window-1] $ \i -> do
        let loopix = (loopix0+i) `mod` 44100
        cur <- Array.readArray loop loopix
        buf <- peekElemOff channels 0
        inp <- peekElemOff buf i
        let new = cur*0.9 + inp
        Array.writeArray loop loopix new
        pokeElemOff buf i new
    modifyIORef ixref (+ fromIntegral window)
hs_looper_exit state = do
    LooperState src dest _ _ <- deRefStablePtr state
    MIDI.stop src
    MIDI.close src
    MIDI.close dest
    freeStablePtr state
