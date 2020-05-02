module MyQueue(Queue(..), printelems, isempty, enqueue, dequeue, show) where




data Queue a = Queue [a]
 deriving (Ord,Eq)

instance Show a => Show (Queue a)
 where
 show (Queue q) = printelems q

data Queue2 a = Queue2 [a] [a]
 deriving (Ord,Eq)

instance Show a => Show (Queue2 a)
 where
 show (Queue2 a b) = printelems2 a b

printelems :: (Show a) => [a] -> String
printelems [] = ""
printelems [x] = show x
printelems (x:xs) = (show x) ++ " - " ++ (printelems xs)

printelems2 :: (Show a) => [a] -> [a] -> String
printelems2 a b = (printelems a) ++ " - " ++ (printelems (reverse b))

isempty :: Queue a -> Bool
isempty (Queue []) = True
isempty _ = False

emptyQ :: Queue a
emptyQ = Queue []

dequeue :: Queue a -> (Maybe a,Queue a)
dequeue (Queue []) = (Nothing, Queue [])
dequeue (Queue (x:xs)) = (Just x, Queue xs)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue q) val = Queue (q ++ [val])

divQ :: Queue a -> Queue2 a
divQ (Queue []) = Queue2 [] []
divQ (Queue [x]) = Queue2 [x] []
divQ (Queue q) =  Queue2 (take hlf q) (reverse (drop hlf q))
 where
 hlf = div (length q +1) 2

refreshdiv :: Queue2 a -> Queue2 a
refreshdiv (Queue2 a b) = divQ (Queue (a ++ (reverse b)))

dualenq :: Queue2 a -> a -> Queue2 a
dualenq (Queue2 a b) val = Queue2 a ([val] ++ b)

dualdq :: Queue2 a -> (Maybe a, Queue2 a)
dualdq (Queue2 [] b) = (Nothing, Queue2 [] b) 
dualdq (Queue2 (a:as) b) = (Just a, Queue2 as b)