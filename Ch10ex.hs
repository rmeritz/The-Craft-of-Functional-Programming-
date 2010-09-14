--The Craft of Functional Programming 
--Ch. 10 Functions as Values

import Prelude hiding (succ, flip, elem, not)
import Ch6ex

--10.1

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

printBill :: TillType -> IO ()
--printBill = putStr . formatBill . makeBill
printBill = makeBill >.> formatBill >.> putStr

--10.2

--(f . id) returns the value of f x

--(id . f) retuns the value of f x  

--f id  is same as (f . id)

--if f::(Int -> Bool)
--(f . id); id :: Int -> Int
--(id . f); id :: Bool -> Bool 
--f id ; id :: Int -> Int

--10.3

composeList :: [(a -> a)] -> (a -> a)
composeList listOfFunctions = foldr (>.>) id listOfFunctions 

--10.4

succ :: Int -> Int
succ n = n+1

iter :: Int -> (a -> a) -> (a -> a)
iter n f 
  | n>0         = f . iter (n-1) f
  | otherwise   = id

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

{-Calculations
iter 3 double 1
double . iter 2 double 1 
double . double . double . iter 1 double 1
double . double . double . id 1 
2* . 2* . 2* . 1
2* . 2* . 2 
2* . 4
8 

(comp2 succ (*)) 3 4
* (succ3) (succ4)
* (3+1) (4+1)
* 4 5
20

comp2 sq add 3 4
add (sq 3) (sq 4)
add (3*3) (4*4)
add 9 16
25
-}

--10.5

tenFive :: Int -> (Int -> Int)  
tenFive = (\n -> iter n succ)

--10.6
{-
f :: (a -> b -> c)
\x y -> f x y  

g :: (b -> a -> c)
\x y -> f y x 
-}

--10.7 

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\x y -> f y x)
 
--10.8

not :: Bool -> Bool 
not = (\x ->
	case x of 
		True -> False
		False -> True)
{-
isWhiteSpace :: Char -> Bool
isWhiteSpace x 
	|\" " -> True
	|\"\n" -> True
	|\"\t" -> True
	|otherwise = False
-}	
--use case syntax

--10.9

total :: (Int -> Int) -> (Int -> Int)
total f = (\x -> sum (map f [0..x]))

--10.10

--Works but not as expected? 
slope :: (Float -> Float) -> (Float -> Float)
slope f = (\x -> (((f (x + 0.0001)) - (f x)) / 0.0001))
	
--10.11

integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f = (\upperbound lowerbound -> (((f upperbound) + (f lowerbound))/ 2)*(upperbound - lowerbound))

--10.12

--comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
--comp2 f g = (\x y -> g (f x) (f y))

comp2PA f g x  = (g (f x)).f    

totalPA f = sum . map f . listFrom0
	where
	listFrom0 :: Int -> [Int]
	listFrom0 n = [0..n]
	
--10.13

--(filter (>0) . map (+1)) is the same as (map (+1) . filter (>=0))

--10.14
