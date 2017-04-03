{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TypeFamilies, ViewPatterns, RecursiveDo #-}

module Sequencer where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified System.MIDI as MIDI

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Foldable (toList)

import Control.Monad.Trans.State
import Data.Word

import Control

type Time = Word32

data SeqState m i o = SeqState
    { seqMidiDevs    :: Devs
    , seqController  :: Either MIDI.MidiMessage o -> m ()
    , seqTimedEvents :: Map.Map Time (m ())
    , seqCondEvents  :: Seq.Seq (i -> m Bool, m ())
    , seqCurrentTime :: Word32
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
    time0 <- liftIO (MIDI.currentTime (fst devs))
    let state0 = SeqState 
            { seqMidiDevs = devs
            -- When we boot, we haven't set up any event handlers yet, so we just ignore
            -- "o" events.
            , seqController = either (liftIO . sendMIDI (snd devs)) (const (return ()))
            , seqTimedEvents = Map.empty
            , seqCondEvents = Seq.empty
            , seqCurrentTime = time0
            }
    (sendO,state1) <- flip runSequencerT state0 . instControl ctrl $ \case
        Left midiEvent -> liftIO (sendMIDI (snd devs) midiEvent)
        Right i -> processEvent i
    return $ state1 { seqController = sendO }

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
    go (Seq.viewl -> (condaction, action) Seq.:< hs) = do
        cond <- getSequencerT (condaction i)
        -- if the condition passes, the action is run then its handler is removed.
        if cond
            then getSequencerT action >> go hs
            else ((condaction,action) Seq.<|) <$> go hs
    go _ = error "impossible non-matching view pattern"

tick :: (MonadIO m) => SequencerT i o m ()
tick = SequencerT $ do
    time <- liftIO . MIDI.currentTime . fst =<< gets seqMidiDevs
    -- run timed events
    iterWhileM (guards ((<= time) . fst . fst) . Map.minViewWithKey <$> gets seqTimedEvents) $
        \((t,action),map') -> do
            -- Pretend the event happened right on time (questionable)
            modify $ \s -> s { seqCurrentTime = t, seqTimedEvents = map' }
            getSequencerT action
    modify $ \s -> s { seqCurrentTime = time }

    -- process incoming MIDI events
    messages <- liftIO . (fmap.fmap) (\(MIDI.MidiEvent _ m) -> m) . MIDI.getEvents . fst 
                    =<< gets seqMidiDevs
    ctrl <- gets seqController
    getSequencerT $ mapM_ (ctrl . Left) messages



when :: (Monad m) => (i -> Bool) -> SequencerT i o m () -> SequencerT i o m ()
when cond = whenM (return . cond)

whenM :: (Monad m) => (i -> SequencerT i o m Bool) -> SequencerT i o m () -> SequencerT i o m ()
whenM cond action = SequencerT $
    modify $ \s -> s { seqCondEvents = seqCondEvents s Seq.|> (cond, action) }

whenever :: (Monad m) => (i -> Bool) -> SequencerT i o m () -> SequencerT i o m ()
whenever cond = wheneverM (return . cond)

wheneverM :: (Monad m) => (i -> SequencerT i o m Bool) -> SequencerT i o m () -> SequencerT i o m ()
wheneverM cond action = whenM cond (action >> wheneverM cond action)

send :: (Monad m) => o -> SequencerT i o m ()
send o = SequencerT $ do
    ctrl <- gets seqController
    getSequencerT (ctrl (Right o))

iterWhileM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
iterWhileM cond action = do
    cond >>= \case
        Nothing -> return ()
        Just x -> action x >> iterWhileM cond action

guards :: (a -> Bool) -> Maybe a -> Maybe a
guards p (Just x) | p x = Just x
guards _ _ = Nothing

findIndexLM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe Int)
findIndexLM p = go . zip [0..] . toList
    where
    go ((i,x):xs) = p x >>= \case
        True -> return (Just i)
        False -> go xs
    go [] = return Nothing
