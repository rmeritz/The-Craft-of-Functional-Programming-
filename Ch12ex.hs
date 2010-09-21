--The Craft of Functional Programing 
--Ch. 12 Overloading Classes and Types 

--12.1

{-/= :: a -> a -> Bool 
/= a b = not (a==b)-}

{---This was the example given in the book and it doesn't work either. Parse error on ..
--Could not find more info an how .. is defined in the Prelude
[m .. n] 
	|m> n = []
	|otherwise = m : [m+1 .. n]-}

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

--I feel very lost. Why would I want to make my own class. 
--My syntax doens't seem to work anyway. 

{-instance (Visible a, Visible b)  => Visible (a, b) where
	toString (a, b)  = (toString a, toString b)
	size _ = 1 -}

{-instance Visible (Bool,Bool,Bool) where
	toString  (True,True,True) = "(True,True,True)"
	toString  (False,True,True) = "(False,True,True)"
	toString  (True,False,True) = "(True,False,True)"
	toString  (False,False,True) = "(False,False,True)" 
	toString  (True,True,False) = "(True,True,False)"
	toString  (False,True,False) = "(False,True,False)"
	toString  (True,False,False) = "(True,False,False)"
	toString  (False,False,False) = "(False,False,False)"
	size _ = 1 	-}

--12.5

instance Visible Int where
	toString int  = show int
  	size _        = 1

--Am I supposed to be using show?

--12.6

--I'm confused. 

--compare :: (Ord a) => a -> a -> Ordering 
--comparer  x y = size x <= size y 

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

--Is this what they are looking for?
--If so it doesn't load. 
--I feel like I'm missing a fundamental concept in this section. 

{-instance (Ord a, Ord b) => Ord (a,b) where 
	compare (x,y) (x',y')
		|(x==x') && (y==y') =EQ
		|(x>x')             =GT
		|(x==x') && (y>y')  =GT
		|otherwise          =LT
		
	(x,y) <= (x',y')          =  compare (x,y) (x',y') /= GT
  	(x,y) < (x',y')           =  compare (x,y) (x',y') == LT
   	(x,y) >= (x',y')          =  compare (x,y) (x',y') /= LT
   	(x,y) > (x',y')           =  compare (x,y) (x',y') == GT

	max (x,y) (x',y')
         | (x,y) <= (x',y')    =  (x',y')
         | otherwise =  (x,y)
    min (x,y) (x',y')
            | (x,y) <= (x',y')    =  (x,y)
         | otherwise =  (x',y')
         
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

--This is the prelude but where is the def. of ord on Bool actually derived? 
--data  Bool  =  False | True     deriving (Eq, Ord, Enum, Read, Show, Bounded)

--True < False == True

--(<) seems to be defined lexiographically on lists of tuples 

--12.10

--showBoolFun :: (Bool -> Bool) -> String 
--I don't understand what this function is supposed to do. (Show the results of the Function as a table)
--Whats an example of a (Bool -> Bool)?

--Same as above
--showBoolFunGen :: (a -> String) -> (Bool -> a) -> String

--12.11

--The instructions for the excercises for this whole chapter seem a little crptic to me. 
--instance Show b => Show (Bool b -> Bool b) where
 