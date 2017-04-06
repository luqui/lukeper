{-# LANGUAGE FlexibleContexts, MultiWayIf #-}

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
    loopdata <- Array.newArray (0,-1) 0
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
        loopdata' <- Array.newArray (0,44099) 0
        writeIORef loopbufref (loopbuf { loopData = loopdata' })
    readIORef loopbufref

stretch :: Double -> Loop -> Loop
stretch factor loop = loop { loopStretchFactor = loopStretchFactor loop * factor }

-- NB. disregards position, assumes append at end of buffer.
-- Quite tricky to get "insert" which is I think what it should do
-- to respect pos.
-- TODO also disrespects loopStretchFactor.
append :: Loop -> Int -> Array.IOUArray Int Double -> IO ()
append loop _ inbuf = do
    loopbuf <- ensureBuffer (loopBuffer loop)
    bufsize <- getSize inbuf
    loopbufsize <- getSize (loopData loopbuf)
    -- NB. reallocates at most once.  *Should* be enough but not technically correct.
    loopdata' <- if loopSize loopbuf + bufsize > loopbufsize
                    then Array.newListArray (0, loopbufsize*2-1) =<< Array.getElems (loopData loopbuf)
                    else return (loopData loopbuf)
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
    let basepos = round (fromIntegral pos / loopStretchFactor loop)
    let grainix = basepos `div` grainsize
    let grainstart = grainsize * grainix
    let modgrainsize = round (fromIntegral grainsize * loopStretchFactor loop)
    let grainoffset = pos - modgrainsize * grainix
    -- XXX if we trace this algorithm with loopStretchFactor = 1, we find some discrepancies with the
    -- xfade terms below.  Removing them makes things smooth in this case, but there are still pops
    -- when loopStretchFactor is nonzero. 
    let samplei = \i -> 
          -- | loopStretchFactor loop < 1 = \i ->
                let subgrainsize = round (fromIntegral grainsize * loopStretchFactor loop) in do
                thissample <- Array.readArray (loopData loopbuf) ((grainstart + grainoffset + i) `mod` loopSize loopbuf)
                if | grainoffset + i < xfade -> do
                    prevsample <- Array.readArray (loopData loopbuf) 
                                ((grainstart - grainsize + subgrainsize - xfade + grainoffset + i) `mod` loopSize loopbuf)
                            --   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^           ^^^^^^^^^^^^^^^^
                            --         end of prev grain                       how far into this grain
                    let delta = fromIntegral (grainoffset + i) / fromIntegral xfade
                    return $ thissample * delta + prevsample * (1 - delta)
                   | subgrainsize - (grainoffset + i) < xfade -> do
                    nextsample <- Array.readArray (loopData loopbuf)
                                ((grainstart + grainsize + grainoffset + i - (subgrainsize - xfade)) `mod` loopSize loopbuf)
                            --   ^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            --      start of next grain       distance from xfade point
                    let delta = fromIntegral (grainoffset + i - (subgrainsize - xfade)) / fromIntegral xfade
                    return $ thissample * (1-delta) + nextsample * delta
                   | otherwise -> Array.readArray (loopData loopbuf) ((grainstart + grainoffset + i) `mod` loopSize loopbuf)
    loopbufsize <- getSize (loopData loopbuf)
    when (loopbufsize /= 0) $ do
        bufsize <- getSize outbuf
        forM_ [0..bufsize-1] $ \i -> do
            cur <- Array.readArray outbuf i
            samp <- samplei i
            Array.writeArray outbuf i (cur + mixout*samp)
    where
    grainsize = round (44100 * 0.1 :: Double)  -- 100ms
    xfade = 500  -- samples


getSize :: (Array.MArray Array.IOUArray a IO) => Array.IOUArray Int a -> IO Int
getSize a = do
    (0, bufEnd) <- Array.getBounds a
    return $ bufEnd+1

