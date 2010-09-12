--The Craft of Functional Programming 
--Ch. 10 Functions as Values

import Prelude hiding (succ)
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

{-
composeList :: [(a -> a)] -> (a -> a)
composeList [] = []
composeList listOfFunctions = foldr1 (>.>) listOfFunctions 
--Trying w/o sucess to compose a list of functions into a single function
-} 

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

