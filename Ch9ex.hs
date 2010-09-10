--The Craft of Functional Programming 
--Ch. 9 Generalization: Patterns of Computation 

import Prelude hiding (length)
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
	ones _ = 1 

--9.3

greaterOne n = n>1 
addOne n = n+1 

--addUp ns = filter greaterOne (map addOne ns)
	
addUp ns = map addOne (filter greaterOne  ns)	

--9.4

nineFour ns = map addOne (map addOne ns)

--9.5

lessTen n = n<10

nineFive ns = filter greaterOne (filter lessTen ns)
 
--9.6

squares xs = (map sq xs)
	where sq x = x*x

sumSq xs = sum (squares xs)

greaterZero n = n > 0 

posList xs = filter greaterZero xs
	
--9.7
--Nothing Works. I think I'm missing some key concept. 

minReturn f n = min (map f [0..n])
--how do I map over a function w/ more than one argurement? Should I be mapping? min isn't of the right type to be solving this problem.  	  
   	  
allEqualReturn (x:xs) =  map (&&) (map (==x) xs)
--Also, I thought there was a bulit in funtion add that performs map (&&) but I cannot find it?

posReturn f n = (filter greaterZero (map f [0..n])) == (map f [0..n])

iSort :: [Int] -> [Int]
iSort []     = [] 
iSort (x:xs) = ins x (iSort xs) 

ins :: Int -> [Int] -> [Int]
ins x []    = [x] 
ins x (y:ys) 
  | x <= y      = x:(y:ys)
  | otherwise   = y : ins x ys

isSorted xs = (xs== iSort xs)

sortReturn f n = isSorted (map f [0..n])

posSortedReturn xs = sortReturn xs && posReturn xs 
	
	

	 