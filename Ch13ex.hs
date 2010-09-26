--The Craft of Functional Programming 
--Ch. 13 Checking Types 

--13.1

{-These functions all don't work because the types aren't consistant

f :: Int -> Int 
f n = 37 +n 
f :: Bool -> Int 
f True = 34

g :: Int -> Int 
g 0 = 37 
g :: a -> Bool 
g n = True 

h :: Int -> ?
h x 
	|x > 0 = True
	|otherwise =37

Pattern matching doesn't work. 
k x = 34
k 0 = 35	
-}	 

--13.2

--(Int -> b) and (a -> Bool) unify to (Int -> Bool)

--(Int, a, a) and (a, a, [Bool]) cannot be unified 

--13.3

--This can be unified (a,[a]) -> (b,c) -> (Bool, [Bool])

--13.4

{-f :: (a, [a]) -> b 
can be applied to (2, [3]), (2,[]), but not (2,[True])-}

--13.5

{-f :: (a, [a]) -> a 
can be applied to (2, [3]), (2,[]), but not (2,[True])-}

--13.6

{-f :: [a] -> [b] -> a -> b 
so
f [] [] :: a -> b 

so
h :: [a] -> (a -> a)
h x = f x x -}

--13.8

curry ::((a,b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a,b) -> c
id :: a -> a 
id:: (b -> c) -> (b -> c)

curry id :: a -> b -> (a,b)
uncurry id :: ((b -> c), b) -> c --Assumes id:: (b -> c) -> (b -> c)
curry (curry id) :: a -> b -> b1 -> ((a,b), b1) --fix--Assumes id :: (a,b) -> (a,b)
uncurry (uncurry id) :: (b -> b1 -> c, b1) -> c --Assumes id:: (b -> c) -> (b -> c)
 