import Data.Char

strLength :: String -> Int
strLength "" = 0
strLength (x:xs) = 1 + strLength(xs)

singleIntToChar :: Int -> [Char]
singleIntToChar n = [chr ((ord '0') + n)]


intToString :: Int -> String
intToString n
 |n < 10 = singleIntToChar n
 |otherwise = intToString (div n 10) ++ singleIntToChar (mod n 10)

firstIndexOf :: Char -> String -> String
firstIndexOf c str
 | (fio c str == strLength str) || (strLength str ==0) = "Not found"
 | otherwise = intToString (fio c str)

fio :: Char -> String -> Int
fio c "" = 0
fio c (x:xs)
 | c == x = 0
 | otherwise = 1 + fio c xs