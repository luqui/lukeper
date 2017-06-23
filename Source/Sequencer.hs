{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TypeFamilies, ViewPatterns, RecursiveDo #-}

module Sequencer where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified System.MIDI as MIDI

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans, lift)

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import Control

data SeqState m i o = SeqState
    { seqMidiDevs    :: Devs
    , seqController  :: Either MIDI.MidiMessage o -> m ()
    , seqTimedEvents :: Map.Map Time (m ())
    , seqCondEvents  :: Seq.Seq (i -> m (Maybe ())) -- Just () to remove, Nothing to keep
    , seqCurrentTime :: Time
    }

newtype SequencerT i o m a 
    = SequencerT { getSequencerT :: StateT (SeqState (SequencerT i o m) i o) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SequencerT i o) where
    lift = SequencerT . lift

instance MonadRefs m => MonadRefs (SequencerT i o m) where
    type Ref (SequencerT i o m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

instance Monad m => MonadSched (SequencerT i o m) where
    now = SequencerT $ gets seqCurrentTime
    at time action = SequencerT $
        modify $ \s -> s { seqTimedEvents = Map.insertWith (>>) time action (seqTimedEvents s) }
    

runSequencerT :: SequencerT i o m a 
              -> SeqState (SequencerT i o m) i o 
              -> m (a, SeqState (SequencerT i o m) i o)
runSequencerT = runStateT . getSequencerT

bootSequencerT :: (MonadIO m) 
               => Devs -> MIDIControl (SequencerT i o m) o i 
               -> m (SeqState (SequencerT i o m) i o)
bootSequencerT devs ctrl = do
    time0 <- liftIO $ (arbitraryPoint ^+^) . fromMillisec . fromIntegral <$> MIDI.currentTime (fst devs)
    let state0 = SeqState 
            { seqMidiDevs = devs
            -- When we boot, we haven't set up any event handlers yet, so we just ignore
            -- "o" events.
            , seqController = either (liftIO . sendMIDI (snd devs)) (const (return ()))
            , seqTimedEvents = Map.empty
            , seqCondEvents = Seq.empty
            , seqCurrentTime = time0
            }
    ((), state1) <- runSequencerT (rebootSequencerT ctrl) state0
    return state1

rebootSequencerT :: (MonadIO m) => MIDIControl (SequencerT i o m) o i -> SequencerT i o m ()
rebootSequencerT ctrl = SequencerT $ do
    devs <- gets seqMidiDevs
    modify (\s -> s
        { seqController = either (liftIO . sendMIDI (snd devs)) (const (return ()))
        , seqTimedEvents = Map.empty
        , seqCondEvents = Seq.empty })
    sendO <- getSequencerT . instControl ctrl $ \case
        Left midiEvent -> liftIO (sendMIDI (snd devs) midiEvent)
        Right i -> processEvent i
    modify (\s -> s { seqController = sendO })
    

sendMIDI :: MIDI.Connection -> MIDI.MidiMessage -> IO ()
-- Why exactly does hmidi reject sysex sends and require a special method?
sendMIDI conn (MIDI.SysEx sysex) = MIDI.sendSysEx conn sysex
sendMIDI conn m = MIDI.send conn m

-- run conditional events  (ignoring their incoming timestamps (questionable))
processEvent :: (Monad m) => i -> SequencerT i o m ()
processEvent i = SequencerT $ do
    handlers <- gets seqCondEvents
    modify (\s -> s { seqCondEvents = Seq.empty })
    handlers' <- go handlers
    modify (\s -> s { seqCondEvents = handlers' })
    where
    go (Seq.viewl -> Seq.EmptyL) = gets seqCondEvents
    go (Seq.viewl -> action Seq.:< hs) = do
        cond <- getSequencerT (action i)
        -- If the action passes, the handler is removed
        case cond of
            Just () -> go hs
            Nothing -> (action Seq.<|) <$> go hs
    go _ = error "impossible non-matching view pattern"

tick :: (MonadIO m) => Int -> SequencerT i o m ()
tick window = SequencerT $ do
    time <- (^+^ fromSamples window) <$> getSequencerT getCurrentTime
    getSequencerT $ do
        loopWhileM (nextEvent time)
        processMidiEvents

processMidiEvents :: (MonadIO m) => SequencerT i o m ()
processMidiEvents = SequencerT $ do
    messages <- liftIO . (fmap.fmap) (\(MIDI.MidiEvent _ m) -> m) . MIDI.getEvents . fst 
                    =<< gets seqMidiDevs
    ctrl <- gets seqController
    getSequencerT $ mapM_ (ctrl . Left) messages

-- Returns True if there are (possibly) more events before the given timestamp.
-- There might not be any, but if not, then the next call will return false.
-- If nextEvent returns True, it had no side effects that call.
nextEvent :: (Monad m) => Time -> SequencerT i o m Bool
nextEvent tmax = SequencerT $ do
    view <- Map.minViewWithKey <$> gets seqTimedEvents
    case view of
        Just ((t,action),map') | t <= tmax -> do
            modify $ \s -> s { seqCurrentTime = t, seqTimedEvents = map' }
            getSequencerT action
            return True
        _ -> do
            time <- getSequencerT getCurrentTime
            modify $ \s -> s { seqCurrentTime = tmax }
            return (time < tmax)

getCurrentTime :: (Monad m) => SequencerT i o m Time
getCurrentTime = SequencerT $ gets seqCurrentTime

-- The general pattern of using when/whenever is:
-- when $ \e -> do
--    SomePattern <- return e
--    lift $ -- what to do if action succeeds
when :: (Monad m) => (i -> MaybeT (SequencerT i o m) ()) -> SequencerT i o m ()
when action = SequencerT . modify $ \s -> s { seqCondEvents = seqCondEvents s Seq.|> (runMaybeT . action) }

whenever :: (Monad m) => (i -> MaybeT (SequencerT i o m) ()) -> SequencerT i o m ()
whenever action = when $ \i -> do
    _ <- lift $ runMaybeT (action i)
    mzero

send :: (Monad m) => o -> SequencerT i o m ()
send o = SequencerT $ do
    ctrl <- gets seqController
    getSequencerT (ctrl (Right o))

loopWhileM :: (Monad m) => m Bool -> m ()
loopWhileM action = do
    r <- action 
    if r then loopWhileM action else return ()
