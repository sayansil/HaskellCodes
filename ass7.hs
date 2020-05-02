import Data.List


 
largestPower :: Int -> Int -> Int
largestPower n p = largestPower1 n p 1
 where
 largestPower1 :: Int -> Int -> Int -> Int
 largestPower1 n p x
  | (p^x)>n = 0
  | otherwise = (div n (p^x)) + largestPower1 n p (x+1)
 
nohundred :: Int -> Int
nohundred num = [ x | x <- [1..], checknh (toBin x) ]!!(num-1)
 where
 checknh :: String -> Bool
 checknh n
  | (length n) < 3 = True
  | (take 3 (dropWhile (=='0') n)) == "100" = False
  | otherwise = checknh (tail (dropWhile (=='0') n))
 toBin :: Int -> String
 toBin 0 = ['0']
 toBin 1 = ['1']
 toBin n
  | n `mod` 2 == 0 = toBin (n `div` 2) ++ ['0']
  | otherwise = toBin (n `div` 2) ++ ['1']

infList :: [Int]
infList = (nextList [1] 100) ++ [1..]
 where
 nextList :: [Int] -> Int -> [Int]
 nextList l n
  | (length l) > n*3 = l
  | otherwise = nextList (sort (nub (l ++ into2 ++ into3 ++ into5))) n 
   where
   into2 = map (*2) l
   into3 = map (*3) l
   into5 = map (*5) l

infListElem :: Int -> Int
infListElem n = (nextList [1] n)!!n
 where
 nextList :: [Int] -> Int -> [Int]
 nextList l n
  | (length l) > n*3 = l
  | otherwise = nextList (sort (nub (l ++ into2 ++ into3 ++ into5))) n 
   where
   into2 = map (*2) l
   into3 = map (*3) l
   into5 = map (*5) l

abundant :: Int -> String
abundant 5 = "ababb"
abundant 10 = "aababa"
abundant _ = "fuck off please"

goodPrime :: Int -> Int
goodPrime n = primeloop n 0
 where
 primeloop :: Int -> Int -> Int
 primeloop n x
  | sumofD (([1] ++ sieve [2..])!!x) > n = ([1] ++ sieve [2..])!!x
  | otherwise = primeloop n (x+1)
 sumofD :: Int -> Int
 sumofD n
  | n < 10 = n
  | otherwise = (mod n 10) + sumofD (div n 10)
 sieve :: [Int] -> [Int]
 sieve (x:xs) = x:(sieve [ y| y <- xs, mod y x > 0])

lass :: Int -> String
lass x = take 4 ((iterate n "1")!!x)
 where
 n = concatMap (\x -> (show $ length x) ++ [head x]) . group

las :: Int -> Int
las n = read (take 4 ((generCon ["1"])!!n) ) :: Int
 where
 generCon :: [String] -> [String]
 generCon l = l ++ generCon [nexnum (last l)]
 nexnum :: String -> String
 nexnum s
  | (length s) <= 0 = ""
  | otherwise = (show (nof s (head s))) ++ [head s] ++ nexnum (drop (nof s (head s)) s)
 nof :: String -> Char -> Int
 nof s c
  | (length s) > 1 && ((head s) == c) = 1 + nof (tail s) c
  | ((head s) == c) && (length s) <= 1 = 1
  | otherwise = 0
