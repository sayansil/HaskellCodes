
gs :: [String]
gs = do{str <- getLine; getStr str [];}


getStr :: String -> [String] -> [String]
getStr s l
 | s == "EOF" = l
 | otherwise = do {str <- getLine; getStr str l;}



rever :: [String] -> [String]
rever l = (drop (div n 2) l) ++ (take (div n 2) l)
 where n = length l