import Criterion.Main
import Data.List
import Data.Char
import qualified Data.Map as M


primes :: [Integer]
primes = mkPrimes 2 M.empty
  where
    mkPrimes n m = case (M.null m, M.findMin m) of
        (False, (n', skips)) | n == n' ->
            mkPrimes (succ n) (addSkips n (M.deleteMin m) skips)
        _ -> n : mkPrimes (succ n) (addSkip n m n)
    addSkip n m s = M.alter (Just . maybe [s] (s:)) (n+s) m
    addSkips = foldl' . addSkip

getList :: [Integer] -> String
getList nums = filter isDigit (show nums)


main :: IO ()
main = do
    putStrLn (getList (take 100 primes))


-- main :: IO ()
-- main = defaultMain [
--     bgroup "Generate List" [ bench "exp 5" $ whnf getList (take 100000 primes),
--                             bench "exp 6" $ whnf getList (take 1000000 primes),
--                             bench "exp 7" $ whnf getList (take 10000000 primes),
--                             bench "exp 8" $ whnf getList (take 100000000 primes)
--                             ]
--     ]
