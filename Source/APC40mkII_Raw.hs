{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}

module APC40mkII_Raw where

-- Types:
--   State of virtual controller
--   State of physical controller
--   Each class of control

-- Methods:
--   Send update of parameter (if necessary)
--   Query state of parameter
--   Detect / route events

-- Handling states is pretty easy I think.  Events are where things get tricky.
-- For example, we might say, "as long as I am on page 1 (of virtual
-- controller), when button 1 is pressed, start recording at the start of the
-- next bar (after which the button means something else)".  Perhaps event
-- handlers are part of the virtual state -- to queue this up, you modify the
-- state to include an event handler.  Then when an event comes in, it suffices
-- to locate the corresponding substate object and manipulate its events.  That
-- actually will work well, since we might want a UI describing event states.
--
-- So we need to read a substate from a state, and we need to write a substate into
-- a state.  And if possible do this in a nuanced way so we don't have to do a million
-- comparisons every tick.  And we should also be able to reset and restore to previous
-- states without much trouble.
--
-- This begs for a lensish thing:  an object expressing the relationship of a
-- state with a substate.  I've been imagining some big data structure describing the state
-- of the system, but perhaps we can keep it more abstract than that and make these lenses
-- the only way of accessing state.

-- We are working with imperative ideas like "change the state of an external
-- device".  I wonder how imperative we should go with these lenses.  Should
-- modify be an IO action?  Without it I am not sure how we are going to get
-- incremental updates, except by building up a data structure of updates
-- DiffArray style.  I don't think that actually buys us anything.
--
-- It looks like it's not parameterized by state at all....  And layers of 
-- abstraction are built just with more complex getters and setters?  Ok,
-- let's see.

import Prelude hiding ((.), id)
import Control.Category

import qualified Data.Array as Array
import qualified System.MIDI.Base as MIDI

import Control


-- MIDI implementation from http://pangolin.com/_Files/APC40Mk2_Communications_Protocol_v1.2.pdf
-- (also included in Resources/).

-- Single RGB Buttons

newtype RGBColor = RGBColor { rgbColorToVel :: Int }
    deriving (Eq, Show)

velToRGBColor :: Int -> RGBColor
velToRGBColor v | 1 <= v && v <= 127 = RGBColor v
                | otherwise = error $ "RGBColor out of range: " ++ show v

data Subdiv = Subdiv24 | Subdiv16 | Subdiv8 | Subdiv4 | Subdiv2
    deriving (Eq, Show, Enum)
data RGBColorState 
    = RGBOff 
    | RGBSolid RGBColor 
    | RGBOneShot Subdiv RGBColor RGBColor  -- start on color1 and gradually transition to color2, then stay at color2
    | RGBPulsing Subdiv RGBColor RGBColor 
    | RGBBlinking Subdiv RGBColor RGBColor
    deriving (Eq, Show)

rgbButton :: (MonadRefs m) => Int -> MIDIControl m RGBColorState Bool
rgbButton note = Control $ \out -> do
    let sendmidi = out . Left
    let sendevent = out . Right
    sendmidi (MIDI.MidiMessage 1 (MIDI.NoteOff note 0))
    return $ \case
        Left midimessage -> getevents sendevent midimessage
        Right colorstate -> senddiff sendmidi colorstate
    where
    senddiff :: (Monad m) => (MIDI.MidiMessage -> m ()) -> RGBColorState -> m ()
    senddiff sendmidi RGBOff = sendmidi $ MIDI.MidiMessage 1 (MIDI.NoteOff note 0)
    senddiff sendmidi (RGBSolid color) = sendmidi $ setPrimary color
    senddiff sendmidi (RGBOneShot  subdiv color1 color2) = do
        sendmidi $ setPrimary color1
        sendmidi $ MIDI.MidiMessage (1+1 +fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2))
    senddiff sendmidi (RGBPulsing  subdiv color1 color2) = do
        sendmidi $ setPrimary color1
        sendmidi $ MIDI.MidiMessage (1+6 +fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2))
    senddiff sendmidi (RGBBlinking subdiv color1 color2) = do
        sendmidi $ setPrimary color1
        sendmidi $ MIDI.MidiMessage (1+11+fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2))
                              --     ^
                              -- MidiMessage channels are 1-based

    setPrimary color = MIDI.MidiMessage 1 (MIDI.NoteOn note (rgbColorToVel color))

    getevents sendevent (MIDI.MidiMessage _ (MIDI.NoteOff note' _)) 
        | note == note' = sendevent False
    getevents sendevent (MIDI.MidiMessage _ (MIDI.NoteOn note' vel))
        | note == note' = sendevent (vel /= 0)
    getevents _ _ = return ()

longRGBButton :: (MonadRefs m, MonadSched m) => Int -> MIDIControl m RGBColorState LongPress
longRGBButton note = right (longPress 500) . rgbButton note

-- The whole matrix of buttons as a single Control

newtype Coord = Coord (Int,Int)
    deriving (Eq, Ord, Show, Array.Ix)

mkCoord :: Int -> Int -> Coord
mkCoord x y 
    | 1 <= x && x <= 8 && 1 <= y && y <= 5 = Coord (x,y)
    | otherwise = error $ "coordinate out of range: " ++ show (x,y)

-- Coord indices are 1-based, and count y from the top.
rgbMatrix :: (MonadRefs m, MonadSched m) => MIDIControl m (Coord, RGBColorState) (Coord, LongPress)
rgbMatrix = Control $ \out -> do
    buttons <- fmap (Array.array (Coord (1,1), Coord (8,5))) . sequence $ do
        coord <- [ Coord (i,j) | i <- [1..8], j <- [1..5] ]
        return . fmap (coord,) . instControl (longRGBButton (coordToNote coord)) $ \case
            Left midi -> out (Left midi)
            Right button -> out (Right (coord, button))
    return $ \case
        Left midi -> getevents buttons midi
        Right (coord, s) -> (buttons Array.! coord) (Right s)
    where
    getevents buttons m@(MIDI.MidiMessage _ (MIDI.NoteOn note _))
        | Just c <- noteToCoord note = (buttons Array.! c) (Left m)
    getevents buttons m@(MIDI.MidiMessage _ (MIDI.NoteOff note _))
        | Just c <- noteToCoord note = (buttons Array.! c) (Left m)
    getevents _ _ = return ()

    coordToNote (Coord (x,y)) = 8*(4-(y-1))+x-1
    noteToCoord n
        | 0 <= n && n < 40 = Just (Coord (n `mod` 8 + 1, 4 - n `div` 8 + 1))
        | otherwise = Nothing

apc40Raw :: (MonadRefs m, MonadSched m) => MIDIControl m (Coord, RGBColorState) (Coord, LongPress)
apc40Raw = Control $ \out -> do
    -- reset device
    out (Left (MIDI.SysEx [
        0x47, 0x7f, 0x29, 0x60, 0x00, 0x04, 
        0x40 + 0x2, --- Ableton live mode 2 (lights don't do shit unless you tell them to)
        0x00, -- version high
        0x00, -- version low
        0x00  -- version bugfix
        ]))
    instControl rgbMatrix out
