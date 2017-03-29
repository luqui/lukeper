{-# LANGUAGE RecordWildCards #-}

module Looper where

import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Array as Foreign

import Control.Monad (forM_)
import System.IO (withFile, IOMode(AppendMode), hPutStrLn)

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
    -> Word32                          -- number of midi messages
    -> Foreign.Ptr MarshalMidiMessage  -- midi data
    -> Foreign.Ptr Word32              -- number of outbound midi messages
    -> IO (Foreign.Ptr MarshalMidiMessage)
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

data LooperState = LooperState Int

hs_looper_init = newStablePtr (LooperState 0)
hs_looper_main state window input output channels midiMessages midiData outMessages = do
    withFile "/tmp/looperlog" AppendMode $ \h -> do
        forM_ [0..fromIntegral midiMessages-1] $ \i -> do
            msg <- peekElemOff midiData i
            hPutStrLn h $ "  " ++ show (mmmSamplePosition msg) ++ " " ++ show (mmmByte1 msg) ++ " " ++ show (mmmByte2 msg) ++ " " ++ show (mmmByte3 msg)
    poke outMessages 0
    Foreign.mallocArray 0
hs_looper_exit state = freeStablePtr state
