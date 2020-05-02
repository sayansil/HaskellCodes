
-- this is the function for xor
xor :: Bool -> Bool -> Bool
xor False b = b
xor b False = b
xor b1 b2 = False

-- this is function for and
myand :: Bool -> Bool -> Bool
myand True b = b
myand b True = b
myand b1 b2 = False

-- this is function for or
or :: Bool -> Bool -> Bool
or False False = False
or b1 b2 = True