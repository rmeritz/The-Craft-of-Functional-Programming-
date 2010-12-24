--The Craft of Functional Programming 
--Ch. 16 Abstract Data Types

--16.1

{-The advantage of a ordered list is it could be more efficent for
finding values. But it could be less efficent for updating. It
could also handle duplicates. -}

module Store ( Store, initial, value, update) where

data Var = String where
instance Ord ... --this is not correct 
data Store = Sto [ (Int, Var) ]

initial :: Store
initial = Sto []

value :: Store -> Var -> Int
value (Sto []) v = 0
value (Sto s@((n,vstored):ss)) v
	| vstored == v = n
	| (secondVar s) < v = value (Sto (first s)) v
	| otherwise = value (Sto (second s)) v

update :: Store -> Var -> Int -> Store
update (Sto s) v n = (Sto (updateL s v n)) 
	where
	updateL :: [ (Int, Var) ] -> Var -> Int -> [ (Int, Var) ] 
	updateL s@((ns,vstored):ss) v n 
		| vstored < v = (n,v):s
		| ((length s) < 2) &&  (vstored > v) = s++[(n,v)]
		| (secondVar s) < v = (updateL (first ss) v n) ++ (second s)
		| otherwise = (first s) ++ (updateL (second s) v n) 

first, second :: [a] -> [a]
first s = take (half s) s
second s = drop (half s) s

half :: [a] -> Int
half s = (length s) `div` 2

secondVar :: [(Int,Var)] -> Var
secondVar s = snd (head (second s))  
