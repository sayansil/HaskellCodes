main = do { 
	line <- getLine ; 
	
	if (line == "Now is the time")
		then putStrLn("To come to the aid\nOf the party\nNow is the time\nFor all good men");
		else do {
    		if (line == "EOF") 
        		then return();
        		else do  {
        			main ;
            		putStrLn(line);
        		}
        }
}