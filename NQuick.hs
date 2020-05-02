{-# LANGUAGE BangPatterns #-}
module NQuick (stuqsort) where


import Data.Array.Base (unsafeRead, unsafeWrite, getNumElements, unsafeNewArray_)
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (when)

stuqsort :: STUArray s Int Int -> ST s ()
stuqsort arr = do
    n <- getNumElements arr
    when (n > 1) (myqsort arr 0 (n-1))

myqsort :: STUArray s Int Int -> Int -> Int -> ST s ()
myqsort a lo hi = do
    p <- unsafeRead a hi
    j <- unstablePartition (< p) lo hi a
    h <- unsafeRead a j
    unsafeWrite a j p
    unsafeWrite a hi h
    when (j > lo+1) (myqsort a lo (j-1))
    when (j+1 < hi) (myqsort a (j+1) hi)

unstablePartition :: (Int -> Bool) -> Int -> Int -> STUArray s Int Int -> ST s Int
unstablePartition f !lf !rg !v = from_left lf rg
  where
    from_left i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v i
                      if f x
                        then from_left (i+1) j
                        else from_right i (j-1)

    from_right i j
      | i == j    = return i
      | otherwise = do
                      x <- unsafeRead v j
                      if f x
                        then do
                               y <- unsafeRead v i
                               unsafeWrite v i x
                               unsafeWrite v j y
                               from_left (i+1) j
                        else from_right i (j-1)