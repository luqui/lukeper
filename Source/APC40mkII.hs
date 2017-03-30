{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, TupleSections, TypeFamilies #-}

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

import qualified Data.Array as Array
import qualified Data.IORef as IORef
import qualified System.MIDI.Base as MIDI
import qualified System.MIDI as MIDI
import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (Monoid(..))

import Control.Monad.Trans.Reader

newtype Seq a = Seq { getSeq :: forall m. Monoid m => (a -> m) -> m }

instance Functor Seq where
    fmap f s = Seq (\sing -> getSeq s (sing . f))

instance Monoid (Seq a) where
    mempty = Seq (const mempty)
    mappend a b = Seq $ getSeq a `mappend` getSeq b

singleton :: a -> Seq a
singleton x = Seq ($ x)


class (Monad m) => MonadMIDI m where
    sendMIDI :: MIDI.MidiMessage -> m ()

class (Monad m) => MonadRefs m where
    type Ref m :: * -> *
    newRef :: a -> m (Ref m a)
    readRef :: Ref m a -> m a
    writeRef :: Ref m a -> a -> m ()


newtype MIDIIO a = MIDIIO { getMIDIIO :: ReaderT MIDI.Connection IO a }
    deriving (Functor, Applicative, Monad)

runMIDIIO :: MIDIIO a -> MIDI.Connection -> IO a
runMIDIIO = runReaderT . getMIDIIO

instance MonadMIDI MIDIIO where
    sendMIDI msg = MIDIIO $ do
        conn <- ask
        liftIO $ MIDI.send conn msg

instance MonadRefs MIDIIO where
    type Ref MIDIIO = IORef.IORef
    newRef = MIDIIO . liftIO . IORef.newIORef
    readRef = MIDIIO . liftIO . IORef.readIORef
    writeRef r x = MIDIIO $ liftIO (IORef.writeIORef r x)


-- Control m d e: d is the type of "diffs" to send, e is the type of events to receive. Notice the state is not explicit.
data Control m d e = Control 
    { sendDiff   :: d -> m ()
    , getEvents  :: MIDI.MidiMessage -> m (Seq e)
    }


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

rgbButton :: (MonadMIDI m, MonadRefs m) => Int -> m (Control m RGBColorState Bool)
rgbButton note = do
    state <- newRef RGBOff
    return $ Control (senddiff state) (getevents state)
    where
    senddiff state s = do
        writeRef state s
        senddiff' s

    senddiff' RGBOff = sendMIDI $ MIDI.MidiMessage 1 (MIDI.NoteOff note 0)
    senddiff' (RGBSolid color) = setPrimary color
    senddiff' (RGBOneShot  subdiv color1 color2) = 
        setPrimary color1 >> sendMIDI (MIDI.MidiMessage (1+1 +fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2)))
    senddiff' (RGBPulsing  subdiv color1 color2) = 
        setPrimary color1 >> sendMIDI (MIDI.MidiMessage (1+6 +fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2)))
    senddiff' (RGBBlinking subdiv color1 color2) =
        setPrimary color1 >> sendMIDI (MIDI.MidiMessage (1+11+fromEnum subdiv) (MIDI.NoteOn note (rgbColorToVel color2)))
                                                  --     ^
                                                  -- MidiMessage channels are 1-based

    setPrimary color = sendMIDI (MIDI.MidiMessage 1 (MIDI.NoteOn note (rgbColorToVel color)))

    getevents state (MIDI.MidiMessage _ (MIDI.NoteOff note' _)) 
        | note == note' = do
            senddiff' =<< readRef state   -- this preserves the color of the button when it is pressed
            return (singleton False)
    getevents state (MIDI.MidiMessage _ (MIDI.NoteOn note' vel))
        | note == note' = do
            senddiff' =<< readRef state   -- preserves button color when it's pressed
            return $ if vel == 0 then singleton False
                                 else singleton True
    getevents _ _ = return mempty


-- The whole matrix of buttons as a single Control

newtype Coord = Coord (Int,Int)
    deriving (Eq, Ord, Show, Array.Ix)

mkCoord :: Int -> Int -> Coord
mkCoord x y 
    | 1 <= x && x <= 8 && 1 <= y && y <= 5 = Coord (x,y)
    | otherwise = error $ "coordinate out of range: " ++ show (x,y)

-- Coord indices are 1-based, and count y from the top.
rgbMatrix :: (MonadMIDI m, MonadRefs m) => m (Control m (Coord, RGBColorState) (Coord, Bool))
rgbMatrix = do
    buttons <- Array.array (Coord (1,1), Coord (8,5)) <$> 
                 mapM (\c -> (c,) <$> rgbButton (coordToNote c)) [ Coord (i,j) | i <- [1..8], j <- [1..5] ]
    return $ Control (senddiff buttons) (getevents buttons)
    where
    senddiff buttons (c, s) = sendDiff (buttons Array.! c) s
    getevents buttons m@(MIDI.MidiMessage _ (MIDI.NoteOn note _))
        | Just c <- noteToCoord note = (fmap.fmap) (c,) (getEvents (buttons Array.! c) m)
    getevents buttons m@(MIDI.MidiMessage _ (MIDI.NoteOff note _))
        | Just c <- noteToCoord note = (fmap.fmap) (c,) (getEvents (buttons Array.! c) m)
    getevents _ _ = return mempty

    coordToNote (Coord (x,y)) = 8*(4-(y-1))+x-1
    noteToCoord n
        | 0 <= n && n < 40 = Just (Coord (n `mod` 8 + 1, 4 - n `div` 8 + 1))
        | otherwise = Nothing

openOutDev :: IO MIDI.Connection
openOutDev = MIDI.openDestination . head =<< filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateDestinations

openInDev :: IO MIDI.Connection
openInDev = do
    src <- fmap head . filterM (fmap ("APC40 mkII" ==) . MIDI.getName) =<< MIDI.enumerateSources
    conn <- MIDI.openSource src Nothing
    MIDI.start conn
    return conn

type Devs = (MIDI.Connection, MIDI.Connection)
openDevs :: IO Devs
openDevs = liftA2 (,) openInDev openOutDev

closeDevs :: Devs -> IO ()
closeDevs (indev, outdev) = do
    MIDI.stop indev
    MIDI.close indev
    MIDI.close outdev

sendC :: Devs -> Control MIDIIO d e -> d -> IO ()
sendC (_,outconn) ctrl d = runMIDIIO (sendDiff ctrl d) outconn

recvC :: Devs -> Control MIDIIO d e -> IO [e]
recvC (inconn,outconn) ctrl = do
    messages <- (fmap.fmap) (\(MIDI.MidiEvent _ m) -> m) $ MIDI.getEvents inconn
    evseq <- runMIDIIO (fmap mconcat (mapM (getEvents ctrl) messages)) outconn
    return $ getSeq evseq (:[])
