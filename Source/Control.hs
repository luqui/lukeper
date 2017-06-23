{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, TupleSections, TypeFamilies, UndecidableSuperClasses #-}

module Control where

import Prelude hiding (id, (.))

import qualified Control.Monad.Trans.State as StateT
import qualified Data.IORef as IORef
import qualified System.MIDI as MIDI

import Control.Applicative (liftA2)
import Control.Monad (filterM, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.ST
import Data.STRef

import Control.Category

import qualified Signals as Sig

newtype Control m i o = Control { instControl :: (o -> m ()) -> m (i -> m ()) }

stateProc :: (MonadRefs m, MonadSched m) 
          => s -> (s -> Maybe a -> Sig.WindowT Time m (s, Maybe b)) 
          -> Control m a b 
stateProc s0 trans = Control $ \out -> do
    stateref <- newRef s0
    
    let update e = do
            state <- readRef stateref
            t <- now
            (endt, (state', event)) <- Sig.runWindowT (trans state e) (t, eternity)
            writeRef stateref state'
            case event of
                Nothing -> return ()
                Just b -> out b
            when (endt /= eternity) $
                at endt (update Nothing)
            
    after zeroV (update Nothing)
    return (update . Just)

statelessProc :: (MonadRefs m, MonadSched m) 
              => (Maybe a -> Sig.WindowT Time m (Maybe b))
              -> Control m a b
statelessProc trans = stateProc () (\_ m -> ((),) <$> trans m)

initialize :: (Monad m) => b -> Control m a b -> Control m a b
initialize b ctrl = Control $ \out -> do
    f <- instControl ctrl out
    out b
    return f

filterProc :: (MonadRefs m, MonadSched m) => (a -> Maybe b) -> Control m a b
filterProc f = statelessProc (\case
    Nothing -> return Nothing
    Just x -> return (f x))

multiOutProc :: (MonadRefs m, MonadSched m) => (a -> [b]) -> Control m a b
multiOutProc f = stateProc [] $ \pending input ->
    let newouts = maybe id (\x -> (++ f x)) input in
    case newouts pending of
        [] -> return ([], Nothing)
        (x:xs) -> Sig.instant (xs, Just x)

data LongPress = PressDown | PressUp | PressLong
    deriving (Eq,Show)

data LongPressState t
    = LPIdle
    | LPWaiting t

longPress :: (MonadSched m, MonadRefs m) => TimeDiff -> Control m Bool LongPress
longPress delay = stateProc LPIdle $ \state e -> do
    (wmin, _) <- Sig.window
    case (e, state) of
        (Nothing, LPIdle) -> return (LPIdle, Nothing)
        (Nothing, LPWaiting t)
            | wmin >= t -> return (LPIdle, Just PressLong)
            | otherwise -> Sig.restrict t >> return (LPWaiting t, Nothing)
        (Just False, _) -> return (LPIdle, Just PressUp)
        (Just True, _) -> 
            let target = wmin ^+^ delay in
            Sig.restrict target >> return (LPWaiting target, Just PressDown)


-- Bizarrely, composition needs Monad, but none of the Arrow combis do (except composition I guess).
instance (Monad m) => Category (Control m) where
    id = Control $ return
    bc . ab = Control $ \outc -> do
        outb <- instControl bc outc
        instControl ab outb

instance Functor (Control m i) where
    fmap f c = Control $ \out -> instControl c (out . f)

-- While Control is not an Arrow because it does not have tuple-carrying operations, 
-- it is an "ArrowChoice", so we implement the combinators without instantiating the class.
arr :: (Applicative m) => (i -> o) -> Control m i o
arr f = Control $ \outo -> pure (outo . f)

(+++) :: (Applicative m) => Control m i o -> Control m i' o' -> Control m (Either i i') (Either o o')
io +++ io' = Control $ \outoo' -> 
    either <$> instControl io (outoo' . Left) <*> instControl io' (outoo' . Right)

left :: (Applicative m) => Control m i o -> Control m (Either i d) (Either o d)
left = (+++ arr id)

right :: (Applicative m) => Control m i o -> Control m (Either d i) (Either d o)
right = (arr id +++)

(|||) :: (Applicative m) => Control m i o -> Control m i' o -> Control m (Either i i') o
io ||| i'o = fmap untag (io +++ i'o)
    where
    untag (Left x) = x
    untag (Right x) = x




class (Monad m) => MonadRefs m where
    type Ref m :: * -> *
    newRef :: a -> m (Ref m a)
    readRef :: Ref m a -> m a
    writeRef :: Ref m a -> a -> m ()

class (Monad m) => MonadSched m where
    now :: m Time
    at :: Time -> m () -> m ()

after :: (MonadSched m) => TimeDiff -> m () -> m ()
after dt action = do
    t <- now
    at (t ^+^ dt) action

instance MonadRefs IO where
    type Ref IO = IORef.IORef
    newRef = IORef.newIORef
    readRef = IORef.readIORef
    writeRef = IORef.writeIORef

instance (MonadRefs m) => MonadRefs (StateT.StateT s m) where
    type Ref (StateT.StateT s m) = Ref m
    newRef = lift . newRef
    readRef = lift . readRef
    writeRef r = lift . writeRef r
    

instance MonadRefs (ST s) where
    type Ref (ST s) = STRef s
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef



class (VectorSpace (Diff v)) => AffineSpace v where
    type Diff v :: *
    (^+^) :: v -> Diff v -> v
    (^-^) :: v -> v -> Diff v

class (AffineSpace v, Diff v ~ v, Num (Scalar v)) => VectorSpace v where
    type Scalar v :: *
    zeroV :: v
    (*^) :: Scalar v -> v -> v

(^/) :: (Fractional (Scalar v), VectorSpace v) => v -> Scalar v -> v
v ^/ s = recip s *^ v

ratio :: TimeDiff -> TimeDiff -> Double
ratio (TimeDiff a) (TimeDiff b) = a / b

-- Unbased makes sure that none of our calculations depend on an absolute notion of "a"
-- (assuming "arbitraryPoint" is used appropriately -- essentially it may only be used once)
newtype Unbased a = Unbased a
    deriving (Eq, Ord, Show)

instance (VectorSpace v) => AffineSpace (Unbased v) where
    type Diff (Unbased v) = v
    Unbased v ^+^ w = Unbased (v ^+^ w)
    Unbased v ^-^ Unbased w = v ^-^ w

arbitraryPoint :: (VectorSpace a) => Unbased a
arbitraryPoint = Unbased zeroV


newtype TimeDiff = TimeDiff Double  -- samples at 44100  (continuified -- there are enough bits in a Double for up to 10^15)
    deriving (Eq, Ord, Show)

instance AffineSpace TimeDiff where
    type Diff TimeDiff = TimeDiff
    TimeDiff a ^+^ TimeDiff b = TimeDiff (a + b)
    TimeDiff a ^-^ TimeDiff b = TimeDiff (a - b)

instance VectorSpace TimeDiff where
    type Scalar TimeDiff = Double
    zeroV = TimeDiff 0
    s *^ TimeDiff a = TimeDiff (s * a)

eternity :: Time
eternity = Unbased (TimeDiff (1/0))

fromSamples :: Int -> TimeDiff
fromSamples = TimeDiff . fromIntegral

fromMillisec :: Double -> TimeDiff
fromMillisec = TimeDiff . (* 44.100)

toSamples :: TimeDiff -> Int
toSamples (TimeDiff s) = round s

type Time = Unbased TimeDiff


type MIDIControl m i o = Control m (Either MIDI.MidiMessage i) (Either MIDI.MidiMessage o)

countEvents :: (MonadRefs m) => Control m i (Int,i)
countEvents = Control $ \out -> do
    countref <- newRef 0
    return $ \i -> do
        count <- readRef countref
        writeRef countref $! count+1
        out (count,i)

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
