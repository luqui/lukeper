{-# LANGUAGE RecordWildCards #-}

module Looper where

import qualified Data.Time.Clock as Clock
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified System.MIDI as MIDI

import Control.Monad (forM_, filterM)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn)

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

data MarshalMidiMessage = MarshalMidiMessage {
    mmmSamplePosition :: Word32,
    mmmByte1 :: Word8,
    mmmByte2 :: Word8,
    mmmByte3 :: Word8 }

instance Storable MarshalMidiMessage where
    alignment _ = 4
    sizeOf _    = 8
    peek ptr  = MarshalMidiMessage <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 5 <*> peekByteOff ptr 6
    poke ptr (MarshalMidiMessage{..}) = do
        pokeByteOff ptr 0 mmmSamplePosition
        pokeByteOff ptr 4 mmmByte1
        pokeByteOff ptr 5 mmmByte2
        pokeByteOff ptr 6 mmmByte3

data LooperState = LooperState 
    { lsMidiSource :: MIDI.Connection
    , lsMidiDest   :: MIDI.Connection
    }

findConnection :: (MIDI.MIDIHasName a) => String -> [a] -> IO a
findConnection name = fmap head . filterM (\c -> (name ==) <$> MIDI.getName c)
    where
    getConn (x:_) = x
    getConn [] = error $ "No MIDI device with name " ++ show name

hs_looper_init = do
    source <- flip MIDI.openSource Nothing =<< findConnection "APC40 mkII" =<< MIDI.enumerateSources
    dest <- MIDI.openDestination =<< findConnection "APC40 mkII" =<< MIDI.enumerateDestinations
    MIDI.start source
    newStablePtr $ LooperState source dest
hs_looper_main state window input output channels = do
    {-
    withFile "/tmp/looperlog" AppendMode $ \h -> do
        forM_ [0..fromIntegral midiMessages-1] $ \i -> do
            msg <- peekElemOff midiData i
            hPutStrLn h $ "  " ++ show (mmmSamplePosition msg) ++ " " ++ show (mmmByte1 msg) ++ " " ++ show (mmmByte2 msg) ++ " " ++ show (mmmByte3 msg)
    -}
    LooperState src dest <- deRefStablePtr state
    time <- floor . realToFrac . Clock.utctDayTime <$> Clock.getCurrentTime
    MIDI.send dest $ MIDI.MidiMessage 0 (MIDI.NoteOn 32 (fromIntegral (time `mod` 128)))
hs_looper_exit state = do
    LooperState src dest <- deRefStablePtr state
    MIDI.stop src
    MIDI.close src
    MIDI.close dest
    freeStablePtr state
