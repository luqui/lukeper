{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, ViewPatterns #-}

module Sequencer where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified System.MIDI as MIDI

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Foldable (toList)
import Data.Monoid ((<>))

import Control.Monad.Trans.State
import Data.Word

import qualified APC40mkII as APC -- just for stubby midi functions

type Time = Word32

data SeqState m i o = SeqState
    { seqMidiDevs    :: APC.Devs
    , seqController  :: APC.Control m o i
    , seqTimedEvents :: Map.Map Time (m ())
    , seqCondEvents  :: Seq.Seq (i -> m Bool, m ())
    , seqCurrentTime :: Word32
    }

newtype SequencerT i o m a 
    = SequencerT { getSequencerT :: StateT (SeqState (SequencerT i o m) i o) m a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (SequencerT i o) where
    lift = SequencerT . lift

newSeqState :: APC.Devs -> APC.Control m o i -> SeqState m i o
newSeqState devs ctrl = SeqState 
    { seqMidiDevs = devs
    , seqController = ctrl
    , seqTimedEvents = Map.empty
    , seqCondEvents = Seq.empty
    , seqCurrentTime = 0  -- will be set by runSequencerT
    }

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

    -- run conditional events  (ignoring their incoming timestamps (also questionable))
    messages <- liftIO . (fmap.fmap) (\(MIDI.MidiEvent _ m) -> m) . MIDI.getEvents . fst 
                    =<< gets seqMidiDevs
    ctrl <- gets seqController
    eventseq <- getSequencerT $ mconcat <$> mapM (APC.getEvents ctrl) messages

    forM_ (APC.getSeq eventseq (:[])) $ \e -> do
        condEvents <- gets seqCondEvents
        findIndexLM (\(p, _) -> getSequencerT (p e)) condEvents >>= \case
            Just ix | (pre, Seq.viewl -> (_, action) Seq.:< post) <- Seq.splitAt ix condEvents -> do
                modify (\s -> s { seqCondEvents = pre <> post })
                getSequencerT action
            _ -> return ()

runSequencerT :: SequencerT i o m a 
              -> SeqState (SequencerT i o m) i o 
              -> m (a, SeqState (SequencerT i o m) i o)
runSequencerT = runStateT . getSequencerT

when :: (Monad m) => (i -> SequencerT i o m Bool) -> SequencerT i o m () -> SequencerT i o m ()
when cond action = SequencerT $
    modify $ \s -> s { seqCondEvents = seqCondEvents s Seq.|> (cond, action) }

whenever :: (Monad m) => (i -> SequencerT i o m Bool) -> SequencerT i o m () -> SequencerT i o m ()
whenever cond action = when cond (action >> whenever cond action)

at :: (Monad m) => Time -> SequencerT i o m () -> SequencerT i o m ()
at time action = SequencerT $
    modify $ \s -> s { seqTimedEvents = Map.insertWith (>>) time action (seqTimedEvents s) }

now :: (Monad m) => SequencerT i o m Word32
now = SequencerT $ gets seqCurrentTime


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
