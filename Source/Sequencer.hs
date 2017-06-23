{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TypeFamilies, ViewPatterns, RecursiveDo #-}

module Sequencer where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified System.MIDI as MIDI

import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans, lift)

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS (RWST, runRWST, ask, local, get, gets, put, modify)

import Control

data SeqState m i o = SeqState
    { seqMidiDevs    :: Devs
    , seqController  :: Either MIDI.MidiMessage o -> m ()
    , seqTimedEvents :: Map.Map Time (m ())
    , seqCondEvents  :: Seq.Seq (i -> m (Maybe ())) -- Just () to remove, Nothing to keep
    }

newtype SequencerT i o m a 
    = SequencerT { getSequencerT :: RWST (Time,Time) () (SeqState (SequencerT i o m) i o) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (SequencerT i o) where
    lift = SequencerT . lift

instance MonadRefs m => MonadRefs (SequencerT i o m) where
    type Ref (SequencerT i o m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r

instance Monad m => MonadSched (SequencerT i o m) where
    now = SequencerT $ fst <$> ask
    at time action = SequencerT $
        modify $ \s -> s { seqTimedEvents = Map.insertWith (>>) time action (seqTimedEvents s) }
    

runSequencerT :: (Monad m)
              => SequencerT i o m a 
              -> (Time,Time)
              -> SeqState (SequencerT i o m) i o 
              -> m (a, SeqState (SequencerT i o m) i o)
runSequencerT m win state = do
    (x,state',()) <- runRWST (getSequencerT m) win state
    return (x,state')

bootSequencerT :: (MonadIO m) 
               => Devs -> MIDIControl (SequencerT i o m) o i 
               -> m (Time, SeqState (SequencerT i o m) i o)
bootSequencerT devs ctrl = do
    time0 <- liftIO $ (arbitraryPoint ^+^) . fromMillisec . fromIntegral <$> MIDI.currentTime (fst devs)
    let state0 = SeqState 
            { seqMidiDevs = devs
            -- When we boot, we haven't set up any event handlers yet, so we just ignore
            -- "o" events.
            , seqController = either (liftIO . sendMIDI (snd devs)) (const (return ()))
            , seqTimedEvents = Map.empty
            , seqCondEvents = Seq.empty
            }
    ((), state1) <- runSequencerT (rebootSequencerT ctrl) (time0, time0) state0
    return (time0, state1)

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

processMidiEvents :: (MonadIO m) => SequencerT i o m ()
processMidiEvents = SequencerT $ do
    messages <- liftIO . (fmap.fmap) (\(MIDI.MidiEvent _ m) -> m) . MIDI.getEvents . fst 
                    =<< gets seqMidiDevs
    ctrl <- gets seqController
    getSequencerT $ mapM_ (ctrl . Left) messages

-- Splits the current window into contiguous subwindows containing no events,
-- and runs the action in each of those subwindows, interleaved with the events
-- that delmit them.
splitWindows :: (Monad m) => SequencerT i o m a -> SequencerT i o m [a]
splitWindows action = SequencerT go
    where
    go = do
        view <- Map.minViewWithKey <$> gets seqTimedEvents
        (tmin,tmax) <- getSequencerT getWindow
        case view of
            Just ((t,event),map') 
                | t == tmin -> do
                    -- No window here, just run the event
                    modify $ \s -> s { seqTimedEvents = map' }
                    local (const (tmin,tmin)) (getSequencerT event)
                    go
                | t <= tmax -> do
                    reses <- local (const (tmin,t)) (runAction 100)
                    (reses++) <$> local (const (t,tmax)) go
            _ | tmin < tmax -> do
                -- There are no events but there is a nonempty window.
                runAction 100
            _ -> do
                -- No events and empty window.
                return []

    -- Run the action. If a new event is created by the action, rewind and try
    -- the action in a smaller window.  Do this at most 100 times just in case 
    -- we get into an infinite regress.
    runAction 0 = error "runAction recursion limit reached"
    runAction limit = do
        prestate <- get
        (tmin,tmax) <- getSequencerT getWindow
        res <- getSequencerT action
        view <- Map.minViewWithKey <$> gets seqTimedEvents
        case view of
            Just ((t,_),_) | t < tmax -> do
                put prestate
                res' <- local (const (tmin,t)) $ runAction (limit - (1 :: Int))
                reses <- local (const (t,tmax)) go
                return (res' ++ reses)
            _ -> return [res]


getCurrentTime :: (Monad m) => SequencerT i o m Time
getCurrentTime = fst <$> getWindow

getWindow :: (Monad m) => SequencerT i o m (Time,Time)
getWindow = SequencerT ask

getWindowSize :: (Monad m) => SequencerT i o m TimeDiff
getWindowSize = do
    (t0, t1) <- getWindow
    return $ t1 ^-^ t0

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
