--The Craft of Functional Programming 
--Ch. 16 Abstract Data Types

--16.1

{-The advantage of a ordered list over a a list is it could be more 
efficent forfinding values.
But it could be less efficent for updating. 
It could also find/delete duplicates quicker. -}

module Store ( Store, initial, value, update) where

type Var = String
--is the best way to represent a Var? 

data Store = Sto [ (Int, Var) ]
--What is the real difference between signitures and
--constructors? 

exStore1 :: Store
exStore1 = Sto [(1, "a"), (2, "b"), (3, "bat"), (3, "cat")]

initial :: Store
initial = Sto []

value :: Store -> Var -> Int
value (Sto []) v = 0
value (Sto s@((n,vstored):ss)) v
	| (Prelude.==) vstored  v = n
	| (secondVar s) > v = value (Sto (first s)) v
	| otherwise = value (Sto (second s)) v

update :: Store -> Var -> Int -> Store
update (Sto s) v n = (Sto (updateL s v n)) 
	where
	updateL :: [ (Int, Var) ] -> Var -> Int -> [ (Int, Var) ] 
	updateL s@((ns,vstored):ss) v n 
		| vstored >= v = (n,v):s
		| ((length s) < 2) &&  (vstored < v) = s++[(n,v)]
		| (secondVar s) > v = (updateL (first s) v n) ++ (second s)
		| otherwise = (first s) ++ (updateL (second s) v n) 

--Are these the most effcient lookups for this data model?

first, second :: [a] -> [a]
first s = take (half s) s
second s = drop (half s) s

half :: [a] -> Int
half s = (length s) `div` 2

secondVar :: [(Int,Var)] -> Var
secondVar s = snd (head (second s)) 

showStore :: Store -> [ (Int, Var) ] 
showStore (Sto s) = s
--This looks like a destructor to me?

--16.2

{-Define an instance of equality for the unordered list
definition of store such that Stores that give the same
value to each variable are equal 
-}

s1 = Sto [(5,"h"), (3, "g")] 
s2 = Sto [(3,"g"), (5, "h")]
s3 = initial 
s4 = Sto [(7, "h")]
s5 = Sto [(5, "h")]

-- s1 == s2 == s3 /= s4 == s5 /= exStore1

instance Eq Store where 
(Sto sto1) == (Sto sto2) = storeEqOf sto1 sto2

storeEqOf :: [(Int, Var)] -> [(Int, Var)] -> Bool 
storeEqOf [] l2 = True
storeEqOf ((n,v):xs) ys  
	| elem v (listVar ys []) 
		= (elem (n,v) ys) && storeEqOf	xs ys
	| otherwise = storeEqOf xs ys
	where 
	listVar :: [(Int, Var)] -> [Var] -> [Var] 
	listVar [] lv = lv
	listVar ((n,v):xs) lv = listVar xs (v:lv) 

{-Why after I define Eq over Store do I need to distiguse
explicatly (Prelude.==) from (Store.==). Shouldn't type
cheaking take care of that? Also why is (==) infix and
(Prelude.==) not? -}

{-Also, how do I turn spell cheak on in vim? And get better
syntax highlighting of Haskell built in functions and type
definitions? I tried downloading a file and putting it in
~/.vim/syntax, but I didn't have on so I had mkdir it
first. Didn't work. Also my maping of Esc to CapsLock was
not permanate. Also how do I paste from the main computer
registar again?-} 

{-There is definately a way of doing this using the list
monad, because this defintion of Eq is close uses the idea
of power sets. I just don't know how to implement it. 
sto1 >>= ((Prelude.==)sto2)) -}

--16.3


