addLeft :: Int -> [Int] -> [Int]
addLeft n arr = n:arr

addRight :: Int -> [Int] -> [Int]
addRight n [] = [n]
addRight n (x:xs) = x:(addRight n xs)

arrLength :: [Int] -> Int
arrLength [] = 0
arrLength (x:xs) = 1 + arrLength xs

attach :: [Int] -> [Int] -> [Int]
attach [] arr2 = arr2
attach (x:xs) arr2 = x:(attach xs arr2)

reverseArr :: [Int] -> [Int]
reverseArr [] = []
reverseArr (x:xs) = (reverse xs) ++ [x]

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:y:ys) = (x<=y) && isAscending ys

isStrictlyAscending :: [Int] -> Bool
isStrictlyAscending [] = True
isStrictlyAscending [x] = True
isStrictlyAscending (x:y:ys) = (x<y) && isAscending ys

isDescending :: [Int] -> Bool
isDescending [] = True
isDescending [x] = True
isDescending (x:y:ys) = (x>=y) && isDescending ys

isStrictlyDescending :: [Int] -> Bool
isStrictlyDescending [] = True
isStrictlyDescending [x] = True
isStrictlyDescending (x:y:ys) = (x>y) && isDescending ys

isAlternating :: [Int] -> Bool
isAlternating [] = True
isAlternating [x] = True
isAlternating [x,y] = True
isAlternating (x:y:z:zs) = ( (x<=y && y>=z) || (x>=y && y<=z) ) && isAlternating (y:z:zs)

isStrictlyAlternating :: [Int] -> Bool
isStrictlyAlternating [] = True
isStrictlyAlternating [x] = True
isStrictlyAlternating [x,y] = x/=y
isStrictlyAlternating (x:y:z:zs) = ( (x<y && y>z) || (x>y && y<z) ) && isAlternating (y:z:zs)

arrSum :: [Int] -> Int
arrSum [] = 0
arrSum (x:xs) = x + arrSum xs