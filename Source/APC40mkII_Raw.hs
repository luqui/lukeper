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

import Control.Monad (forM_)

import qualified Control.Arrow as Arrow
import qualified Data.Array as Array
import qualified System.MIDI.Base as MIDI

import Control

data Void
absurd :: Void -> a
absurd x = x `seq` error "absurd"

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
longRGBButton note = right (longPress (fromMillisec 500)) . rgbButton note

inputOnlyButton :: (Monad m) => Int -> MIDIControl m Void Bool
inputOnlyButton note  = Control $ \out -> do
    let inproc (Left (MIDI.MidiMessage _ (MIDI.NoteOn note' vel)))
            | note == note'  = out (Right (vel /= 0))
        inproc (Left (MIDI.MidiMessage _ (MIDI.NoteOff note' _)))
            | note == note'  = out (Right False)
        inproc _ = return ()
    return inproc

monoButton :: (Monad m) => Int -> Bool -> MIDIControl m Bool Bool
monoButton note val0 = Control $ \out -> do
    let inproc (Left (MIDI.MidiMessage _ (MIDI.NoteOn note' vel)))
            | note == note'  = out (Right (vel /= 0))
        inproc (Left (MIDI.MidiMessage _ (MIDI.NoteOff note' _)))
            | note == note'  = out (Right False)
        inproc (Right True)  = out (Left (MIDI.MidiMessage 1 (MIDI.NoteOn note 127)))
        inproc (Right False) = out (Left (MIDI.MidiMessage 1 (MIDI.NoteOff note 0)))
        inproc _ = return ()
    -- reset button
    inproc (Right val0)
    return inproc

channelMonoButton :: (Monad m) => Int -> Bool -> MIDIControl m (Int,Bool) (Int,Bool)
channelMonoButton note val0 = Control $ \out -> do
    let inproc (Left (MIDI.MidiMessage ch (MIDI.NoteOn note' vel)))
            | note == note'       = out (Right (ch, vel /= 0))
        inproc (Left (MIDI.MidiMessage ch (MIDI.NoteOff note' _)))
            | note == note'       = out (Right (ch, False))
        inproc (Right (ch,True))  = out (Left (MIDI.MidiMessage ch (MIDI.NoteOn note 127)))
        inproc (Right (ch,False)) = out (Left (MIDI.MidiMessage ch (MIDI.NoteOff note 0)))
        inproc _ = return ()
    forM_ [1..8] $ \ch -> inproc (Right (ch, val0))
    return inproc

relativeControl :: (Monad m) => Int -> MIDIControl m Void Int
relativeControl cc = Control $ \out -> do
    return $ \case
        Left (MIDI.MidiMessage _ (MIDI.CC cc' val)) | cc == cc' ->
            if val < 64 then out (Right val)
                        else out (Right (val-128))
        _ -> return ()

dial :: (Monad m) => Int -> MIDIControl m Int Int
dial cc = Control $ \out -> do
    let inproc (Left (MIDI.MidiMessage _ (MIDI.CC cc' val)))
            | cc == cc' = out (Right val)
        inproc (Right val) = out (Left (MIDI.MidiMessage 1 (MIDI.CC cc val)))
        inproc _ = return ()
    inproc (Right 0)
    return inproc

-- dial ids are 1 based
dials :: (Monad m) => MIDIControl m (Int, Int) (Int, Int)
dials = Control $ \out -> do
    controls <- mapM (\ctrl -> instControl (arr (Arrow.right (ctrl,)) . dial (0x10+ctrl-1)) out) [1..8]
    return $ \case
        Left msg@(MIDI.MidiMessage _ (MIDI.CC cc _))
            | 0x10 <= cc && cc <= 0x17 -> (controls !! (cc - 0x10)) (Left msg)
        Right (ctrl, val)
            | 1 <= ctrl && ctrl <= 8 -> (controls !! (ctrl-1)) (Right val)
        _ -> return ()

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

faders :: (Monad m) => MIDIControl m Void (Int, Double)  -- (fader id (1..9), level [0,1])
faders = Control $ \out -> do
    return $ \case
        Left (MIDI.MidiMessage ch (MIDI.CC 0x07 v)) -> out (Right (ch, fromIntegral v / 127))
        Left (MIDI.MidiMessage _  (MIDI.CC 0x0e v)) -> out (Right (9, fromIntegral v / 127))
        _ -> return ()
    
data APCOutMessage
    = OutMatrixButton Coord LongPress
    | OutFader Int Double
    | OutTempoChange Int
    | OutMetronome Bool
    | OutSessionButton Bool
    | OutUnmuteButton Int Bool
    | OutStopAllButton Bool
    | OutDial Int Int
    deriving (Eq)

data APCInMessage
    = InMatrixButton Coord RGBColorState
    | InMetronome Bool
    | InSessionButton Bool
    | InUnmuteButton Int Bool
    | InClock
    | InDial Int Int
    deriving (Eq)

apc40Raw :: (MonadRefs m, MonadSched m) => MIDIControl m APCInMessage APCOutMessage
apc40Raw = Control $ \out -> do
    -- reset device
    out (Left (MIDI.SysEx [
        0x47, 0x7f, 0x29, 0x60, 0x00, 0x04, 
        0x40 + 0x2, --- Ableton live mode 2 (lights don't do shit unless you tell them to)
        0x00, -- version high
        0x00, -- version low
        0x00  -- version bugfix
        ]))
    matrixI <- instControl rgbMatrix (out . Arrow.right (uncurry OutMatrixButton))
    fadersI <- instControl faders (out . Arrow.right (uncurry OutFader))
    tempoI <- instControl (relativeControl 0x0d) (out . Arrow.right OutTempoChange)
    metronomeI <- instControl (monoButton 0x5a False) (out . Arrow.right OutMetronome)
    sessionI <- instControl (monoButton 0x66 False) (out . Arrow.right OutSessionButton)
    unmuteI <- instControl (channelMonoButton 0x32 True) (out . Arrow.right (uncurry OutUnmuteButton))
    stopAllI <- instControl (inputOnlyButton 0x51) (out . Arrow.right OutStopAllButton)
    dialsI <- instControl dials (out . Arrow.right (uncurry OutDial))
    return $ \case 
        Left midi -> do
            matrixI (Left midi)
            fadersI (Left midi)
            tempoI (Left midi)
            metronomeI (Left midi)
            sessionI (Left midi)
            unmuteI (Left midi)
            stopAllI (Left midi)
            dialsI (Left midi)
        Right (InMatrixButton coord state) -> matrixI (Right (coord, state))
        Right (InMetronome b) -> metronomeI (Right b)
        Right (InSessionButton b) -> sessionI (Right b)
        Right (InUnmuteButton ch b) -> unmuteI (Right (ch,b))
        Right InClock -> out (Left MIDI.SRTClock)
        Right (InDial ctrl val) -> dialsI (Right (ctrl, val))
