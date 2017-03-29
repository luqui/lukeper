{-# LANGUAGE ConstraintKinds, KindSignatures, RankNTypes, TypeFamilies #-}

module APC40mkII where

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

import qualified System.MIDI.Base as MIDI
import Data.Kind (Constraint)
import Data.Monoid (Monoid(..))

class MonadStates (m :: * -> * -> *) where
    type Product m :: (* -> * -> *) -> Constraint
    getS :: m s s
    setS :: s -> m s ()
    productS :: (Product m p) => m s a -> m s' b -> m (p s s') (a,b)

newtype Seq a = Seq { getSeq :: forall m. Monoid m => (a -> m) -> m }

instance Monoid (Seq a) where
    mempty = Seq (const mempty)
    mappend a b = Seq $ getSeq a `mappend` getSeq b

singleton :: a -> Seq a
singleton x = Seq ($ x)

data Control s = Control 
    { toMIDI         :: s -> Seq MIDI.MidiMessage
    , updateFromMIDI :: MIDI.MidiMessage -> Maybe (s -> s)  -- Nothing represents id
    }

-- MIDI implementation from http://pangolin.com/_Files/APC40Mk2_Communications_Protocol_v1.2.pdf
-- (also included in Resources/).

newtype RGBColor = RGBColor { rgbColorToVel :: Int }
    deriving (Eq, Show)

velToRGBColor :: Int -> RGBColor
velToRGBColor v | 1 <= v && v <= 127 = RGBColor v
                | otherwise = error $ "RGBColor out of range: " ++ show v

data Subdiv = Subdiv24 | Subdiv16 | Subdiv8 | Subdiv4 | Subdiv2
    deriving (Eq, Show, Enum)
data RGBDynamic = Primary | OneShot Subdiv | Pulsing Subdiv | Blinking Subdiv
    deriving (Eq, Show)
data RGBState = RGBOff | RGBState RGBColor RGBDynamic
    deriving (Eq, Show)

rgbButton :: Int -> Control RGBState
rgbButton note = Control to from
    where
    to RGBOff = singleton (MIDI.MidiMessage 1 (MIDI.NoteOff note 0))
    to (RGBState color dyn) = singleton (MIDI.MidiMessage (1+dynToChannel dyn) (MIDI.NoteOn note (rgbColorToVel color)))

    from (MIDI.MidiMessage ch (MIDI.NoteOff note' _)) 
        | note == note' = Just (const RGBOff)
    from (MIDI.MidiMessage ch (MIDI.NoteOn note' vel))
        | note == note' = Just . const $
            if vel == 0 then RGBOff
                        else RGBState (velToRGBColor vel) (channelToDyn ch)
    
    dynToChannel Primary = 0
    dynToChannel (OneShot  subdiv) = 1 + fromEnum subdiv
    dynToChannel (Pulsing  subdiv) = 6 + fromEnum subdiv
    dynToChannel (Blinking subdiv) = 11 + fromEnum subdiv

    channelToDyn c
        | c == 0 = Primary
        | c < 6  = OneShot (toEnum (c-1))
        | c < 11 = Pulsing (toEnum (c-6))
        | c < 16 = Blinking (toEnum (c-11)) 
        | otherwise = error $ "Invalid channel: " ++ show c
