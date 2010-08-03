--Ch 3 Basic Types and Definintions

module Ch3exercises where

import Prelude hiding (min)

--3.1

exOr :: Bool -> Bool -> Bool 
exOr x y = (x &&  (not y)) || ((not x) && y)

--3.3

{-
exOr True True = False
exOr True False = True
exOr False True = True
exOr False False = False
-}

--3.4 

nAnd :: Bool -> Bool -> Bool
nAnd x y = not(x && y)

--3.6

mystery :: Int -> Int -> Int -> Bool
mystery m n p = not ((m==n) && (n==p))

--3.7

threeDifferent :: Int -> Int -> Int-> Bool
threeDifferent m n p = not((m==n)||(m==p))

--3.8

fourEqual :: Int -> Int-> Int -> Int-> Bool
--fourEqual m n p q = (m==n) && (m==p) && (m==q)

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (m==p)

fourEqual m n p q = threeEqual m n p && threeEqual n p q

--3.11 

min :: Int -> Int -> Int 
min x y 
	|x <=y     =x
	|otherwise =y 
	
minThree :: Int -> Int -> Int -> Int 
minThree x y z
	| x <= y && x<= z  =x
	| y <=z            =y
	| otherwise 	   =z
	
--3.12

ord :: Char -> Int
ord  =  fromEnum

chr  :: Int  -> Char
chr  =  toEnum

offset :: Int
offset = ord 'A' - ord 'a'


toUpper :: Char -> Char
toUpper ch
	|(ord ch) <=90 	=ch
	|otherwise	=chr(ord ch + offset)


--3.13

charToNum :: Char -> Int 
charToNum num
	|(ord num) <48 || (ord num) >57    =0
	|otherwise     			   =(ord num - 48)
	
--3.14

averageThree :: Int-> Int -> Int -> Float
averageThree a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int 
howManyAboveAverage a b c
	|(((fromIntegral a) > (averageThree a b c)) `exOr` ((fromIntegral b) > (averageThree a b c))) `exOr` ((fromIntegral c) > (averageThree a b c))    = 1
	| otherwise															                  = 2

--3.15

numberDRoots :: Float -> Float -> Float -> Int
numberDRoots a b c
	| (b^2) >  (4.0*a*c)   =2
	| (b^2) == (4.0*a*c)   =1
	| (b^2) <  (4.0*a*c)   =0
	
numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
	| not (a==0.0)              = numberDRoots a b c 
	| not (b==0.0)		    = 1
	| b == 0.0 && not (c==0.0)  = 0 
	| b == 0.0 && c==0          = 3

root1:: Float -> Float -> Float -> Float
root1 a b c = ((-b + sqrt((b^2)-4*a*c))/ (2*a))

root2:: Float -> Float -> Float -> Float
root2 a b c = ((-b - sqrt((b^2)-4*a*c))/ (2*a))
	
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
	| (numberRoots a b c == 0) || (numberRoots a b c == 3)    = 0.0 
	| root1 a b c >= root2 a b c                              = root2 a b c 
	| otherwise						  = root1 a b c


--3.19

funny x= x+x
peculiar y=y



