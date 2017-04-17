{-# LANGUAGE BangPatterns, FlexibleContexts, MultiWayIf #-}

module Loop 
    ( AudioBuffer
    , Loop
    , newLoop
    , getLoopSize
    , stretch

    , play
    , append
    )
where

import qualified Data.Array.Base as Array
import qualified Data.Array.IO as Array

import Control.Monad (forM_, when)

import Data.IORef

type AudioBuffer = Array.IOUArray Int Double

data LoopBuffer = LoopBuffer
    { loopSize :: Int
    , loopData :: Array.IOUArray Int Double
    }

data Loop = Loop
    { loopBuffer :: IORef LoopBuffer
    , loopStretchFactor :: Double
    }

newLoopBuffer :: IO LoopBuffer
newLoopBuffer = do
    loopdata <- Array.newArray_ (0,-1)
    return $ LoopBuffer { loopSize = 0, loopData = loopdata }
    
newLoop :: IO Loop
newLoop = do
    buf <- newIORef =<< newLoopBuffer
    return $ Loop { loopBuffer = buf, loopStretchFactor = 1 }

getLoopSize :: Loop -> IO Int
getLoopSize loop = do
    basesize <- loopSize <$> readIORef (loopBuffer loop)
    return $ floor (fromIntegral basesize * loopStretchFactor loop)

ensureBuffer :: IORef LoopBuffer -> IO LoopBuffer
ensureBuffer loopbufref = do
    loopbuf <- readIORef loopbufref
    bufsize <- getSize (loopData loopbuf)
    when (bufsize == 0) $ do
        loopdata' <- Array.unsafeNewArray_ (0,44100-1)
        writeIORef loopbufref (loopbuf { loopData = loopdata' })
    readIORef loopbufref

stretch :: Double -> Loop -> Loop
stretch factor loop = loop { loopStretchFactor = loopStretchFactor loop * factor }

reallocToFit :: Int -> Array.IOUArray Int Double -> IO (Array.IOUArray Int Double)
reallocToFit fit buf = do
    size <- getSize buf
    if fit > size
        then do
            let !len = 2*size-1
            buf' <- Array.unsafeNewArray_ (0,len)
            let fillFrom !i | i == len  = return buf'
                            | otherwise = do Array.unsafeWrite buf' i =<< Array.unsafeRead buf i
                                             fillFrom (i+1)
            fillFrom 0
        else return buf

-- NB. disregards position, assumes append at end of buffer.
-- Quite tricky to get "insert" which is I think what it should do
-- to respect pos.
-- TODO also disrespects loopStretchFactor.
append :: Loop -> Int -> Array.IOUArray Int Double -> IO ()
append loop _ inbuf = do
    loopbuf <- ensureBuffer (loopBuffer loop)
    bufsize <- getSize inbuf
    loopdata' <- reallocToFit (loopSize loopbuf + bufsize) (loopData loopbuf)
    forM_ [0..bufsize-1] $ \i -> do
        Array.writeArray loopdata' (loopSize loopbuf + i) =<< Array.readArray inbuf i
    writeIORef (loopBuffer loop) $  
        loopbuf { loopSize = loopSize loopbuf + bufsize
                , loopData = loopdata'
                }

play :: Loop -> Int -> Double -> Array.IOUArray Int Double -> IO ()
play loop pos mixout outbuf
    | loopStretchFactor loop == 1 = playUnstretched loop pos mixout outbuf
    | otherwise = playStretched loop pos mixout outbuf

playUnstretched :: Loop -> Int -> Double -> Array.IOUArray Int Double -> IO ()
playUnstretched loop pos mixout outbuf = do
    loopbuf <- readIORef (loopBuffer loop)
    loopsize <- getSize (loopData loopbuf)
    when (loopsize /= 0) $ do
        bufsize <- getSize outbuf
        forM_ [0..bufsize-1] $ \i -> do
            cur <- Array.readArray outbuf i
            samp <- Array.readArray (loopData loopbuf) ((pos + i) `mod` loopSize loopbuf)
            Array.writeArray outbuf i (cur + mixout*samp)

playStretched :: Loop -> Int -> Double -> Array.IOUArray Int Double -> IO ()
playStretched loop pos mixout outbuf = do
    loopbuf <- readIORef (loopBuffer loop)
    let basepos = fromIntegral pos / loopStretchFactor loop
    let grainix = fromIntegral (floor (basepos / grainsize) :: Int)
    let grainstart = grainsize * grainix
    let modgrainsize = grainsize * loopStretchFactor loop
    let grainoffset = fromIntegral pos - modgrainsize * grainix
    let samplei = \i -> 
          -- | loopStretchFactor loop < 1 = \i ->
                let subgrainsize = grainsize * loopStretchFactor loop in do
                thissample <- sampleArray loopbuf (grainstart + grainoffset + i)
                if | grainoffset + i < xfade -> do
                    prevsample <- sampleArray loopbuf
                                 (grainstart - grainsize + subgrainsize + grainoffset + i)
                            --   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^
                            --         end of prev grain                 how far into this grain
                    let delta = (grainoffset + i) / xfade
                    return $ thissample * delta + prevsample * (1 - delta)
                   | otherwise -> sampleArray loopbuf (grainstart + grainoffset + i)
    loopbufsize <- getSize (loopData loopbuf)
    when (loopbufsize /= 0) $ do
        bufsize <- getSize outbuf
        forM_ [0..bufsize-1] $ \i -> do
            cur <- Array.readArray outbuf i
            samp <- samplei (fromIntegral i)
            Array.writeArray outbuf i (cur + mixout*samp)
    where
    grainsize = 44100 * 0.1 :: Double  -- 100ms
    xfade = 500  -- samples
    sampleArray loopbuf ix = avg <$> Array.readArray (loopData loopbuf) (floor   ix `mod` loopSize loopbuf) 
                                 <*> Array.readArray (loopData loopbuf) (ceiling ix `mod` loopSize loopbuf)
    avg x y = 0.5 * (x + y)
    


getSize :: (Array.MArray Array.IOUArray a IO) => Array.IOUArray Int a -> IO Int
getSize a = do
    (0, bufEnd) <- Array.getBounds a
    return $ bufEnd+1

