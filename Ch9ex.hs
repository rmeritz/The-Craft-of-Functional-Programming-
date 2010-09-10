--The Craft of Functional Programming 
--Ch. 9 Generalization: Patterns of Computation 

import Prelude hiding (length)
import Ch7ex hiding (and)

--9.1
{-
by primative recursion 
doubleAll [2,1,7]
2*2 : doubleAll [1,7]
2*2 : 2*1 : doubleAll [7]
2*2 : 2*1 : 2*7 : doubleAll []
2*2 : 2*1 : 2*7 : []
4 : 2 : 14 : []
[4,2,14] 

by list comprehension
doubleAll [2,1,7]
[4,2,14]

by map
doubleAll [2,1,7] 
map double [2,1,7]
double 2 : map double [1,7]
double 2 : double 1 : map double [7]
double 2 : double 1 : double 7 : map double []
double 2 : double 1 : double 7 : []
2*2 : 2*1 : 2*7 : []
4 : 2 : 14 : []
[4,2,14]
-}

--9.2

length :: [a] -> Int 
length xs = sum (map ones xs)
	where
	ones :: a -> Int
	ones _ = 1 

--9.3
greaterOne :: Int -> Bool 
greaterOne n = n>1 

addOne :: Int -> Int
addOne n = n+1 

addUp :: [Int] -> [Int]
addUp ns = filter greaterOne (map addOne ns)
	
addUp1 :: [Int] -> [Int]	
addUp1 ns = map addOne (filter greaterOne  ns)	

--9.4
nineFour :: [Int] -> [Int]
nineFour ns = map addOne (map addOne ns)

--9.5

lessTen :: Int -> Bool
lessTen n = n<10

nineFive :: [Int] -> [Int]
nineFive ns = filter greaterOne (filter lessTen ns)
 
--9.6

squares :: [Int] -> [Int]
squares xs = (map sq xs)
	where
	sq :: Int -> Int 
	sq x = x*x

sumSq :: [Int] -> Int
sumSq xs = sum (squares xs)

greaterZero :: Int -> Bool  
greaterZero n = n > 0 

posList :: [Int] -> [Int]
posList xs = filter greaterZero xs
	
--9.7

--map :: (a -> b) -> [a] -> [b]
--filter :: (a -> bool) -> [a] -> [a] 
--zipWidth :: (a -> b_-> c) -> [a] -> [b] -> [c]

minList :: [Int] -> Int 
minList [x] = x
minList (x:xs)
	|x < minList xs = x
	|otherwise = minList xs 

minReturn :: (Int -> Int) -> Int -> Int 
minReturn f n = minList (map f [0..n])
--Is there a way to replace minList with higher-order function? All of the higher order functions return a list and I want tp return a value. But could I somehow return a list of only one element and then give that element? I could filter with x< but I would need to update x. How could I do that? Or I could compare two lists with zipWidth one having the min and the other the rest of the list. But again the lists would have to be updated. 

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = and (map (==(f 0)) (map f [0..n]))

posReturn :: (Int -> Int) -> Int -> Bool 
posReturn f n = length (map (>0) (map f [0..n])) == length (map f [0..n])		 
--Is there a better way to do this than with the length function?

isSorted :: [Int] -> Bool 
isSorted xs = (xs == iSort xs)

isSortedAndPosReturn :: (Int -> Int) -> Int -> Bool 
isSortedAndPosReturn f n = (posReturn f n) && (isSorted (map f [0..n])) 

 