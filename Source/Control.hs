{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Control where

import Prelude hiding (id, (.))

import qualified Data.IORef as IORef
import qualified System.MIDI as MIDI

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Data.Word (Word32)

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
    now :: m Word32
    -- millisecs
    at :: Word32 -> m () -> m ()

after :: (MonadSched m) => Word32 -> m () -> m ()
after dt action = do
    t <- now
    at (t + dt) action

instance MonadRefs IO where
    type Ref IO = IORef.IORef
    newRef = IORef.newIORef
    readRef = IORef.readIORef
    writeRef = IORef.writeIORef


type MIDIControl m i o = Control m (Either MIDI.MidiMessage i) (Either MIDI.MidiMessage o)

data LongPress = PressDown | PressUp | PressLong
    deriving (Eq, Show)

countInputs :: (MonadRefs m) => Control m (Int,i) o -> Control m i o
countInputs c = Control $ \out -> do
    countref <- newRef 0
    sendI <- instControl c out
    return $ \i -> do
        count <- readRef countref
        writeRef countref $! count+1
        sendI (count,i)

countOutputs :: (MonadRefs m) => Control m i o -> Control m i (Int,o)
countOutputs c = Control $ \out -> do
    countref <- newRef 0
    instControl c $ \o -> do
        count <- readRef countref
        writeRef countref $! count+1
        out (count,o)

longPress :: (MonadRefs m, MonadSched m) => Word32 -> Control m Bool LongPress
longPress delay = countInputs . Control $ \out -> do
    downcountref <- newRef 0
    return $ \(count, i) -> do
        if i then do writeRef downcountref count
                     after delay $ do
                         downcount <- readRef downcountref
                         if count == downcount then out PressLong else return ()
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