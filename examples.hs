import Data.Char
import Data.List
-- this is the code for gcd using Euclid's method


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


egcd :: Int -> Int -> Int
egcd a 0 = a
egcd a b
 | a < b = egcd b a
 | otherwise = egcd b (mod a b)


-- this is the parent code for highest divisor

highestdiv :: Int -> Int
highestdiv n = hdiv n (n-1)

-- this is the code for highest divisor

hdiv :: Int -> Int -> Int
hdiv x y
 | mod x y == 0 = y
 | otherwise = hdiv x (y-1)

-- test function1
function1 :: Int -> Int -> Char -> Bool
function1 a 0 c = True
function1 0 b c = False
function1 a b c = function1 (a-1) (b-1) c

f :: Int -> Int
f n = g n (n+1)

g :: Int -> Int -> Int
g m i
 | (mod i m) == 0 = i
 | otherwise = g m (i+1)

h :: Int -> Int -> Int
h m 0 = m
h m n = h (div m 10) (10*n + (mod m 10))

arrayLength :: [Int] -> Int
arrayLength [] = 0
arrayLength (h:t) = 1 + arrayLength t

arraySum :: [Int] -> Int
arraySum [] = 0
arraySum (x:xs) = x + arraySum xs




type Point = (Float,Float)

dist :: Point -> Point -> Float
dist (x1,y1) (x2,y2) = 
 let diffxsq = (x1-x2)*(x1-x2)
     diffysq = (y1-y2)*(y1-y2)
 in sqrt (diffxsq + diffysq)

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