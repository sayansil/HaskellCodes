repeated :: String -> [String] -> Int
repeated s [] =0
repeated s (x:xs)
 | s == x = 1 + repeated s xs
 | otherwise = repeated s xs

highestf :: [String] -> Int -> String
highestf [] n = n
highestf (x:xs) n 
 | (repeated x (x:xs)) > n = highestf xs (repeated x (x:xs)) 
 | otherwise = highestf xs n