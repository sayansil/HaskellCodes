import Data.List

ramanujan :: Int -> Int
ramanujan n = head (drop (n-1) [1729, 4104, 13832, 20683, 32832, 39312, 40033, 46683, 64232, 65728, 110656, 110808, 134379, 149389, 165464, 171288, 195841, 216027, 216125, 262656, 314496, 320264, 327763])

is_square_matrix :: [[a]] -> Bool
is_square_matrix l
 | length (concat l) == 0 = False
 | otherwise = (length l) == ((length (head l))) && is_mymatrix l

is_mymatrix :: [[a]] -> Bool
is_mymatrix [] = True
is_mymatrix [x] = True
is_mymatrix (x:y:ys) = (length x == length y) && is_mymatrix ys

is_matrix :: [[a]] -> Bool
is_matrix l
 | length (concat l) == 0 = False
 | otherwise = is_mymatrix l

addable :: [[Int]] -> [[Int]] -> Bool
addable m n
 | (not (is_matrix n)) || (not (is_matrix m)) = False
 | (length m == length n) && (length (head m) == length (head n)) = True
 | otherwise = False

multiplyable :: [[Int]] -> [[Int]] -> Bool
multiplyable m n
 | (not (is_matrix n)) || (not (is_matrix m)) = False
 | length (head m) == length n = True
 | otherwise = False

multiply_matrix :: [[Int]] -> [[Int]] -> [[Int]]
multiply_matrix [] _ = []
multiply_matrix m n = [multi (head m) (transpose n)]++multiply_matrix (tail m) n
 where
	multi :: [Int] -> [[Int]] -> [Int]
	multi _ [] = []
	multi m n = [sum (zipWith (*) m (head n))]++multi m (tail n)