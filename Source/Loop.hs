{-# LANGUAGE FlexibleContexts #-}

module Loop 
    ( AudioBuffer
    , LoopState(..)
    , Loop
    , newLoop
    , runLoop
    , clearLoop
    , setLoopPos

    , getLoopState
    , setLoopState
    , modifyLoopState
    )
where

import qualified Data.Array.IO as Array

import Control.Monad (forM_, when)

import Data.IORef

type AudioBuffer = Array.IOUArray Int Double

data LoopState = Disabled | Appending | Playing
    deriving (Eq, Show)

data LoopBuffer = LoopBuffer
    { loopSize :: Int
    , loopPos  :: Int
    , loopData :: Array.IOUArray Int Double
    , loopState :: LoopState
    }

newtype Loop = Loop (IORef LoopBuffer)

newLoopBuffer :: IO LoopBuffer
newLoopBuffer = do
    loopdata <- Array.newArray (0,-1) 0
    return $ LoopBuffer { loopSize = 0, loopPos = 0, loopData = loopdata, loopState = Disabled }
    
newLoop :: IO Loop
newLoop = do
    fmap Loop . newIORef =<< newLoopBuffer

runLoop :: Loop -> AudioBuffer -> Double -> AudioBuffer -> IO ()
runLoop (Loop loopbufref) inbuf mixout outbuf = do
    loopbuf <- readIORef loopbufref
    case loopState loopbuf of
        Disabled -> return ()
        Appending -> append loopbufref inbuf
        Playing 
            | loopSize loopbuf /= 0 -> play loopbufref mixout outbuf
            | otherwise             -> return ()

setLoopState :: Loop -> LoopState -> IO ()
setLoopState (Loop loopbufref) loopstate = do
    loopbuf <- readIORef loopbufref
    writeIORef loopbufref (loopbuf { loopState = loopstate })

getLoopState :: Loop -> IO LoopState
getLoopState (Loop loopbufref) = loopState <$> readIORef loopbufref

modifyLoopState :: Loop -> (LoopState -> LoopState) -> IO ()
modifyLoopState (Loop loopbufref) f = modifyIORef loopbufref (\b -> b { loopState = f (loopState b) })

-- Move to purer implementation
clearLoop :: Loop -> IO ()
clearLoop (Loop loopbufref) = writeIORef loopbufref =<< newLoopBuffer

setLoopPos :: Loop -> Int -> IO ()
setLoopPos (Loop loopbufref) pos = modifyIORef loopbufref (\loop -> loop { loopPos = pos `wrap` loopSize loop })
    where
    _ `wrap` 0 = 0
    x `wrap` m = x `mod` m


ensureBuffer :: IORef LoopBuffer -> IO LoopBuffer
ensureBuffer loopbufref = do
    loopbuf <- readIORef loopbufref
    bufsize <- getSize (loopData loopbuf)
    when (bufsize == 0) $ do
        loopdata' <- Array.newArray (0,44099) 0
        writeIORef loopbufref (loopbuf { loopData = loopdata' })
    readIORef loopbufref

append :: IORef LoopBuffer -> Array.IOUArray Int Double -> IO ()
append loopbufref inbuf = do
    loopbuf <- ensureBuffer loopbufref
    bufsize <- getSize inbuf
    loopbufsize <- getSize (loopData loopbuf)
    -- NB. reallocates at most once.  *Should* be enough but not technically correct.
    loopdata' <- if loopSize loopbuf + bufsize > loopbufsize
                    then Array.newListArray (0, loopbufsize*2-1) =<< Array.getElems (loopData loopbuf)
                    else return (loopData loopbuf)
    forM_ [0..bufsize-1] $ \i -> do
        Array.writeArray loopdata' (loopSize loopbuf + i) =<< Array.readArray inbuf i
    writeIORef loopbufref $  
        loopbuf { loopSize = loopSize loopbuf + bufsize
                , loopPos  = 0
                , loopData = loopdata'
                }

play :: IORef LoopBuffer -> Double -> Array.IOUArray Int Double -> IO ()
play loopbufref mixout outbuf = do
    loopbuf <- readIORef loopbufref
    bufsize <- getSize outbuf
    forM_ [0..bufsize-1] $ \i -> do
        cur <- Array.readArray outbuf i
        samp <- Array.readArray (loopData loopbuf) ((loopPos loopbuf + i) `mod` loopSize loopbuf)
        Array.writeArray outbuf i (cur + mixout*samp)
    writeIORef loopbufref $ loopbuf { loopPos = loopPos loopbuf + bufsize `mod` loopSize loopbuf }


getSize :: (Array.MArray Array.IOUArray a IO) => Array.IOUArray Int a -> IO Int
getSize a = do
    (0, bufEnd) <- Array.getBounds a
    return $ bufEnd+1

