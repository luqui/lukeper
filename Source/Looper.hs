module Looper where

foreign export ccall foo :: Int -> Int

foo :: Int -> Int
foo = succ
