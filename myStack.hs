
data Stack a = Stack [a] 
 deriving (Ord, Eq)

instance (Show a) => Show (Stack a)
 where
 show (Stack l) = printStack l

printStack :: (Show a) => [a] -> String
printStack [] = ""
printStack [x] = show x
printStack (x:xs) = (show x) ++ " - " ++ (printStack xs)

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

isempty :: Stack a -> Bool
isempty (Stack []) = True
isempty _ = False

emptystack :: Stack a
emptystack = (Stack [])

sumStack :: (Num a) => Stack a -> a
sumStack (Stack []) = 0
sumStack (Stack (x:xs)) = x + sumStack (Stack xs)

insertindexeff :: Stack a -> a -> Int -> Stack a
insertindexeff (Stack l) val 0 = Stack (val:l)
insertindexeff (Stack (x:xs)) val n = concatStack (Stack [x])  (insertindexeff (Stack xs) val (n-1))

insertindex :: Stack a -> a -> Int -> Stack a
insertindex (Stack l) val n = Stack ((take n l) ++ [val] ++ (drop n l))

concatStack :: Stack a -> Stack a -> Stack a
concatStack (Stack x) (Stack y) = Stack (x ++ y)