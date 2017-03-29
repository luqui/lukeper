{-# LANGUAGE RecordWildCards #-}

module Looper where

import qualified Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Array as Foreign
import Foreign.StablePtr
import Foreign.Storable

foreign export ccall hs_looper_init :: IO (StablePtr LooperState)
foreign export ccall hs_looper_main 
    :: StablePtr LooperState
    -> Int                             -- window size
    -> Int                             -- input channels
    -> Int                             -- output channels
    -> Foreign.Ptr (Foreign.Ptr Float) -- channel data
    -> Int                             -- number of midi messages
    -> Foreign.Ptr MarshalMidiMessage  -- midi data
    -> Foreign.Ptr Int                 -- number of outbound midi messages
    -> IO (Foreign.Ptr MarshalMidiMessage)
foreign export ccall hs_looper_exit :: StablePtr LooperState -> IO ()

data MarshalMidiMessage = MarshalMidiMessage {
    mmmSamplePosition :: Int,
    mmmByte1 :: Foreign.Word8,
    mmmByte2 :: Foreign.Word8,
    mmmByte3 :: Foreign.Word8 }

instance Storable MarshalMidiMessage where
    alignment _ = 8
    sizeOf _    = 16
    peek ptr  = MarshalMidiMessage <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 9 <*> peekByteOff ptr 10
    poke ptr (MarshalMidiMessage{..}) = do
        pokeByteOff ptr 0 mmmSamplePosition
        pokeByteOff ptr 8 mmmByte1
        pokeByteOff ptr 9 mmmByte2
        pokeByteOff ptr 10 mmmByte3

data LooperState = LooperState Int

hs_looper_init = newStablePtr (LooperState 0)
hs_looper_main state window input output channels midiMessages midiData outMessages = do
    poke outMessages 0
    Foreign.mallocArray 0
hs_looper_exit state = freeStablePtr state
