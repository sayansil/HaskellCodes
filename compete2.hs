import System.Environment (getArgs)
import System.CPUTime
import System.Random
import Text.Printf

import Data.Array.Unboxed
import Data.Array.ST hiding (unsafeThaw)
import Data.Array.Unsafe (unsafeThaw)
import Data.Array.Base (unsafeRead, unsafeWrite, getNumElements, unsafeNewArray_)
import Control.Monad.ST
import Control.Monad (when)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM





















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
unstablePartition f lf rg v = from_left lf rg
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













nextR :: StdGen -> (Int, StdGen)
nextR = randomR (minBound, maxBound)

buildArray :: StdGen -> Int -> UArray Int Int
buildArray sg size = runSTUArray (do
    arr <- unsafeNewArray_ (0, size-1)
    let fill i g
            | i < size  = do
                let (r, g') = nextR g
                unsafeWrite arr i r
                fill (i+1) g'
            | otherwise = return arr
    fill 0 sg)

buildVector :: StdGen -> Int -> U.Vector Int
buildVector sg size = U.fromList $ take size (randoms sg)

time :: IO a -> IO ()
time action = do
    t0 <- getCPUTime
    action
    t1 <- getCPUTime
    let tm :: Double
        tm = fromInteger (t1 - t0) * 1e-9
    printf "%.3f ms\n" tm

stu :: UArray Int Int -> Int -> IO ()
stu ua sz = do
    let sa = runSTUArray (do
                st <- unsafeThaw ua
                stuqsort st
                return st)
    forM_ [0, sz `quot` 2, sz-1] (print . (sa `unsafeAt`))


main :: IO ()
main = do
    args <- getArgs
    let num = case args of
                 (a:_) -> read a
                 _ -> 1000000
    sg <- getStdGen
    let ar = buildArray sg num
        algos = [("STUArray", stu ar)]
    forM_ algos $ \(name, act) -> do
        putStrLn name
        time (act num)

-- For the prevention of sharing
foo :: Int -> Int
foo n
    | n < 0 = -n
    | n > 0 = n
    | otherwise = 3