--The Craft of Functional Programming 
--Ch. 11 Program Development 

--11.1 

{---This was the example given in the book and it doesn't work either. Parse error on ..
--Could not find more info an how .. is defined in the Prelude
-- Its built in but I don't think I can hide it.
[m .. n] 
	|m> n = []
	|otherwise = m : [m+1 .. n]-}

----Bad syntax? Need Emum.
--[m,n .. p]
-- |(p-n)> (n-m) = []
-- |otherwise = [m] ++ [n,(n+n-m) .. p]
 
 --11.2 
 
simplePalCheck :: String -> Bool
simplePalCheck st = (reverse st == st)

palCheck :: String -> Bool 
palCheck = simplePalCheck . allLower

charctures = ['!','-', ':', ';', '.', '"', '`', ',', '?', '\'']

whitespace = ['\n', '\t',' ']

ord :: Char -> Int
ord  =  fromEnum

chr  :: Int  -> Char
chr  =  toEnum

offset :: Int
offset = ord 'A' - ord 'a'

allLower :: String -> String
allLower "" = "" 
allLower (c:ch)
  |elem c charctures || elem c whitespace  = allLower ch 
  |ord c <= 90 = (chr(ord c - offset)): allLower ch 
  |otherwise = c: allLower ch 
  
--11.3

--See 7.6



  