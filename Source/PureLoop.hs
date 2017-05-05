{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances #-}

module PureLoop 
    ( Loop
    , fromVector
    , length
    , addRange
    , index
    , indexRange
    )
where

import Prelude hiding (length)
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad(..))
import qualified Data.FingerTree as FingerTree
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Monoid (Sum(..))

newtype LoopChunk = LoopChunk (Vector.Vector Double)

type Measure = Sum Int

instance FingerTree.Measured Measure LoopChunk where
    measure (LoopChunk v) = Sum (Vector.length v)

newtype Loop = Loop { getLoop :: FingerTree.FingerTree Measure LoopChunk }
    deriving (Monoid)

fromVector :: Vector.Vector Double -> Loop
fromVector = Loop . FingerTree.singleton . LoopChunk

length :: Loop -> Int
length = getSum . FingerTree.measure . getLoop

addRange :: (PrimMonad m) => Int -> Int -> Int -> Loop -> Double -> MVector.MVector (PrimState m) Double -> m ()
addRange loopstart bufstart len loop mixout target = do
    let index0 = loopstart `mod` length loop
    let (prefix, postfix) = FingerTree.split ((> index0) . getSum) (getLoop loop)
    go postfix (index0 - getSum (FingerTree.measure prefix)) bufstart len
    where
    go _ _ _ 0 = return ()
    go bufseq ix bufpos lenleft = 
        case FingerTree.viewl bufseq of
            FingerTree.EmptyL -> go (getLoop loop) ix bufpos lenleft
            LoopChunk buf FingerTree.:< bufs
                | ix >= Vector.length buf -> go bufs (ix - Vector.length buf) bufpos lenleft
                | otherwise -> do
                    let innerlen = min lenleft (Vector.length buf - ix)
                    forM_ [0..innerlen-1] $ \i -> do
                        samp <- MVector.unsafeRead target (bufpos+i)
                        MVector.unsafeWrite target (bufpos+i) (samp + mixout*(buf Vector.! (ix+i)))
                    go (LoopChunk buf FingerTree.<| bufs) (ix+innerlen) (bufpos+innerlen) (lenleft-innerlen)

index :: Int -> Loop -> Double
index i loop = case FingerTree.viewl postfix of
                    FingerTree.EmptyL -> error $ "Impossible empty view of postfix at index " ++ show i ++ " (prefix size " ++ show (FingerTree.measure prefix) ++ " ; postfix size = " ++ show (FingerTree.measure postfix) ++ ")"
                    LoopChunk buf FingerTree.:< _ 
                        | i'' < Vector.length buf -> buf Vector.! i''
                        | otherwise -> error $ "Index out of bounds: " ++ show i'' ++ " in " ++ show (Vector.length buf)
    where
    (prefix, postfix) = FingerTree.split ((> i') . getSum) (getLoop loop)
    i' = i `mod` length loop
    i'' = i' - getSum (FingerTree.measure prefix)

indexRange :: Int -> Int -> Loop -> Double -> Vector.Vector Double
indexRange loopstart len loop mixout = Vector.create $ do
    buf <- MVector.new len
    addRange loopstart 0 len loop mixout buf
    return buf
