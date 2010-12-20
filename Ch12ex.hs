--The Craft of Functional Programing 
--Ch. 12 Overloading Classes and Types 

--12.1

{-(/=) :: a -> a -> Bool 
(/=) a b = not (a==b)-}


--12.2

numEqual :: (Eq a) => a -> [a] -> Int 
numEqual x xs = length (filter (==x) xs)
 
--12.3

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs) 
	|p x = x : filterFirst p xs
	|otherwise = xs

--oneLookUpFirst :: (Eq a) => [(a,b)] -> a -> b
oneLookUpFirst l a = snd (head(filterFirst ((==a).fst) l))

--oneLookUpSecond :: (Eq a) => [(a,b)] -> b -> a
oneLookUpSecond l b = fst (head(filterFirst ((==b).snd) l))

--12.4

class Visible a where
  toString :: a -> String
  size     :: a -> Int

instance Visible Char where
  toString ch  = [ch]
  size _       = 1
 
instance Visible Bool where
  toString True  = "True"
  toString False = "False"
  size _         = 1

instance (Visible a, Visible b)  => Visible (a, b) where
	toString (a, b)  = "("++(toString a)++","++(toString b)++")"
	size _ = 2

instance (Visible a, Visible b, Visible c) => Visible (a, b, c) where
	toString  (a, b, c) = "(" ++ (toString a)++ ", "++ (toString b) ++ ", "++ (toString c)++ ")"
	size _ = 3

--12.5

digits :: Int -> [Int]
digits n
	|(mod n 10) /= n = digits (div n 10) ++ [mod n 10]
	|otherwise = [n] 

instance Visible Int where
	toString 0 = "0"
	toString 1 = "1"
	toString 2 = "2"
	toString 3 = "3"
	toString 4 = "4"
	toString 5 = "5"
	toString 6 = "6"
	toString 7 = "7"
	toString 8 = "8"
	toString 9 = "0"
	toString n = foldr1 (++) (map toString (digits n))
  	size _        = 1

--12.6

comparer :: (Visible a, Visible a1) => a -> a1 -> Bool 
comparer  x y = size x <= size y 

--12.7

{-class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

    max x y 
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y-}

--12.7

{-
instance (Ord a, Ord b) => Ord (a,b) where 
	compare (x,y) (x',y')
		|(x==x') && (y==y') =EQ
		|(x>x')             =GT
		|(x==x') && (y>y')  =GT
		|otherwise          =LT
		-}
{-	(x,y) <= (x',y')          =  compare (x,y) (x',y') /= GT
  	(x,y) < (x',y')           =  compare (x,y) (x',y') == LT
   	(x,y) >= (x',y')          =  compare (x,y) (x',y') /= LT
   	(x,y) > (x',y')           =  compare (x,y) (x',y') == GT-}

{-	max (x,y) (x',y')
         | (x,y) <= (x',y')    =  (x',y')
         | otherwise =  (x,y)
    min (x,y) (x',y')
            | (x,y) <= (x',y')    =  (x,y)
         | otherwise =  (x',y')-}
{-      
instance Ord b => Ord [b] where
	compare _ [] = GT
	compare [] _ = LT
	compare (x:xs) (y:ys)
		|compare x y =/EQ = compare x y 
		|otherwise = compare xs ys 

	(x:xs) <= (y:ys)          =  compare (x:xs) (y:ys) /= GT
  	(x:xs) < (y:ys)            =  compare (x:xs) (y:ys) == LT
   	(x:xs) >= (y:ys)           =  compare (x:xs) (y:ys) /= LT
   	(x:xs) > (y:ys)            =  compare (x:xs) (y:ys) == GT
	
	max (x:xs) (y:ys)
    	| (x:xs) <= (y:ys)    =  (y:ys)
        | otherwise =  (x:xs)
    min (x:xs) (y:ys)
    	| (x:xs) <= (y:ys)    =  (x:xs)
        | otherwise =  (y:ys)-}

--12.9

--data  Bool  =  False | True     deriving (Eq, Ord, Enum, Read, Show, Bounded)

--True < False == True

--(<) seems to be defined lexiographically on lists of tuples 
 