main :: IO ()
main = do
 line <- getLine
 if (line == "EOF") then do
  return ()
  else do
   let processedLine = reverse line
   main
   putStrLn (processedLine)
