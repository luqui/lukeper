{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Control where

import Prelude hiding (id, (.))

import qualified Control.Monad.Trans.State as StateT
import qualified Data.IORef as IORef
import qualified System.MIDI as MIDI

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)

import Control.Category


newtype Control m i o = Control { instControl :: (o -> m ()) -> m (i -> m ()) }


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
    -- millisecs
    at :: Time -> m () -> m ()

after :: (MonadSched m) => TimeDiff -> m () -> m ()
after dt action = do
    t <- now
    at (add t dt) action

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


-- Unbased makes sure that none of our calculations depend on an absolute notion of "a"
-- (assuming "arbitraryPoint" is used appropriately -- essentially it may only be used once)
newtype Unbased a = Unbased a
    deriving (Eq, Ord, Show)

diff :: (Num a) => Unbased a -> Unbased a -> a
diff (Unbased x) (Unbased y) = x - y

add :: (Num a) => Unbased a -> a -> Unbased a
add (Unbased x) y = Unbased (x + y)

arbitraryPoint :: (Num a) => Unbased a
arbitraryPoint = Unbased 0


newtype TimeDiff = TimeDiff Double  -- samples at 44100  (continuified -- there are enough bits in a Double for up to 10^15)
    deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac)

fromSamples :: Int -> TimeDiff
fromSamples = TimeDiff . fromIntegral

fromMillisec :: Double -> TimeDiff
fromMillisec = TimeDiff . (* 44.100)

toSamples :: TimeDiff -> Int
toSamples (TimeDiff s) = round s


type Time = Unbased TimeDiff


type MIDIControl m i o = Control m (Either MIDI.MidiMessage i) (Either MIDI.MidiMessage o)

data LongPress = PressDown | PressUp | PressLong
    deriving (Eq, Show)

countEvents :: (MonadRefs m) => Control m i (Int,i)
countEvents = Control $ \out -> do
    countref <- newRef 0
    return $ \i -> do
        count <- readRef countref
        writeRef countref $! count+1
        out (count,i)

longPress :: (MonadRefs m, MonadSched m) => TimeDiff -> Control m Bool LongPress
longPress delay = go . countEvents
    where
    go = Control $ \out -> do
        lastcountref <- newRef 0
        return $ \(count, i) -> do
            writeRef lastcountref count
            if i then do after delay $ do
                             lastcount <- readRef lastcountref
                             if count == lastcount then out PressLong else return ()
                         out PressDown
                 else out PressUp


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
