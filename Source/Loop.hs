{-# LANGUAGE BangPatterns, FlexibleContexts, MultiWayIf #-}

module Loop 
    ( Loop
    , newLoop
    , getLoopSize
    , stretch

    , play
    , append
    )
where

import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

import qualified PureLoop as PL

import Control.Monad (when, forM_)
import Control.Monad.Primitive (RealWorld)
import Data.Monoid (Monoid(..), (<>))

import Data.IORef

type MVector = MVector.MVector RealWorld
type LoopBuffer = PL.Loop

data Loop = Loop
    { loopBuffer :: IORef LoopBuffer
    , loopStretchFactor :: Double
    }
    
newLoop :: IO Loop
newLoop = do
    buf <- newIORef mempty
    return $ Loop { loopBuffer = buf, loopStretchFactor = 1 }

getLoopSize :: Loop -> IO Int
getLoopSize loop = do
    basesize <- PL.length <$> readIORef (loopBuffer loop)
    return $ floor (fromIntegral basesize * loopStretchFactor loop)

stretch :: Double -> Loop -> Loop
stretch factor loop = loop { loopStretchFactor = loopStretchFactor loop * factor }

-- NB. disregards position, assumes append at end of buffer.
-- Quite tricky to get "insert" which is I think what it should do
-- to respect pos.
-- TODO also disrespects loopStretchFactor.
append :: Loop -> Int -> Vector.Vector Double -> IO ()
append loop _ inbuf = modifyIORef (loopBuffer loop) (<> PL.fromVector inbuf)

play :: Loop -> Int -> Double -> MVector Double -> IO ()
play loop pos mixout outbuf
    | loopStretchFactor loop == 1 = playUnstretched loop pos mixout outbuf
    | otherwise = playStretched loop pos mixout outbuf

playUnstretched :: Loop -> Int -> Double -> MVector Double -> IO ()
playUnstretched loop pos mixout outbuf = do
    loopbuf <- readIORef (loopBuffer loop)
    PL.addRange pos 0 (MVector.length outbuf) loopbuf mixout outbuf

playStretched :: Loop -> Int -> Double -> MVector Double -> IO ()
playStretched loop pos mixout outbuf = do
    loopbuf <- readIORef (loopBuffer loop)
    let basepos = fromIntegral pos / loopStretchFactor loop
    let grainix = fromIntegral (floor (basepos / grainsize) :: Int)
    let grainstart = grainsize * grainix
    let modgrainsize = grainsize * loopStretchFactor loop
    let grainoffset = fromIntegral pos - modgrainsize * grainix
    let samplei = \i ->
          -- | loopStretchFactor loop < 1 = \i ->
                let subgrainsize = grainsize * loopStretchFactor loop
                    thissample = sampleArray loopbuf (grainstart + grainoffset + i) in
                if | grainoffset + i < xfade -> 
                    let prevsample = sampleArray loopbuf
                                 (grainstart - grainsize + subgrainsize + grainoffset + i)
                            --   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^
                            --         end of prev grain                 how far into this grain
                        delta = (grainoffset + i) / xfade in
                    thissample * delta + prevsample * (1 - delta)
                   | otherwise -> sampleArray loopbuf (grainstart + grainoffset + i)
    let loopbufsize = PL.length loopbuf
    when (loopbufsize /= 0) $ do
        let bufsize = MVector.length outbuf
        forM_ [0..bufsize-1] $ \i -> do
            cur <- MVector.unsafeRead outbuf i
            let samp = samplei (fromIntegral i)
            MVector.unsafeWrite outbuf i (cur + mixout*samp)
    where
    grainsize = 44100 * 0.1 :: Double  -- 100ms
    xfade = 500  -- samples
    sampleArray loopbuf ix = avg (PL.index (floor ix) loopbuf)
                                 (PL.index (ceiling ix) loopbuf)
    avg x y = 0.5 * (x + y)
