--The Craft of Functional Programming 
--Ch. 16 Abstract Data Types

--16.1

{-The advantage of a ordered list over a a list is it could be more 
efficent forfinding values.
But it could be less efficent for updating. 
It could also find/delete duplicates quicker. -}

module Store ( Store, initial, value, update) where

type Var = String

data Store = Sto [ (Int, Var) ]

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

{-
instance Eq Store where
(Sto sto1) == (Sto sto2) = storeEqOf sto1 sto2
-}

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
cheaking take care of that? -}

{-There is definately a way of doing this using the list
monad, because this defintion of Eq is close uses the idea
of power sets. I just don't know how to implement it. 
sto1 >>= ((Prelude.==)sto2)) -}

--16.3

--data Maybe a = Nothing | Just a

--Use maybe to propagate errors in Store- 

--For list implementation only need to change value

valueE :: Store -> Var -> Maybe Int 
valueE (Sto []) v = Nothing
valueE (Sto ((n,vstored):sto)) v
	| v == vstored = Just n
	| otherwise = valueE (Sto sto) v 

--For functional implemenation need to modify type too

newtype StoreF = StoF ( Var -> Maybe Int ) 

initialF :: StoreF
initialF = StoF (\v -> Nothing) 

valueF :: StoreF -> Var -> Maybe Int
valueF (StoF sto) v = sto v

updateF :: StoreF -> Var -> Int -> StoreF
updateF (StoF sto) v n = StoF (\w -> if v==w then Just n else sto w)
--not sure about my use of just in this context. 

--Once I incorperate Maybe how do I get an error? 
--Or is that just what Nothing means by defult?

{-Is there a way to test it completely without
defining an instance of a show-like function over the type? 
What behviors should this function have?
Is there an accepted way to show "Nothing"?-}

{-Is there a syntax creating tests in Haskell similar to
the functionality of Racket tests ? -}  

--valueF (updateF initialF "a" 5) "a" => Just 5
--valueF (updateF initialF "a" 5) "b" => Nothing

--16.4

{-Instead of an error provide a test that shows 
if a variable is in a given store. 
Also, how would the signiture need to be modifed to 
provide a test? -} 

testVal :: Store -> Var -> Bool 
testVal (Sto []) v = False
testVal (Sto ((n,vstored):sto)) v
	| v == vstored = True
	| otherwise = testVal (Sto sto) v

--Is this what is meant by a test?

--16.5

--setAll should set every variable to the value n

--Defined for Store represented by a list:

--setAll :: Int -> Store
--setAll n = Sto [( n, _ )]

--Can/How can I actually do this? 
--I don't thinks it is possible

--Defined for Store represent by a function:

setAllF :: Int -> StoreF
setAllF n = (StoF (\v -> Just n)) 

--valueF (setAllF 5) "a" => Just 5

--16.6

--Design a ADT for the library database
{-I wanted to make it based on functions bc that is more
confusing to me. -}

type Person = String
type Book = String
type Borrowers = [Person]

newtype Database = DB (Book -> Maybe Borrowers) 
{-I don't think this works because I need to get
information in both directions. (See books)
(Book -> [Person]) and (Person -> [Book])
How to I deal with the functions when there are
multiple answers. (books & borrowers)
I mean a [Person] not just a Person. 
Is there a way I can implement this if my
database is a function? Or does it have to be list based?-}

newDB :: Database 
newDB = DB (\b -> Nothing) 

books :: Database -> Person -> Maybe [Book] 
books (DB entries) person = --? (\p if p==person then  

borrowers :: Database -> Book -> Maybe Borrowers 
borrowers (DB entries) book = entries book

borrowed :: Database -> Book -> Bool 
borrowed (DB entries) book 
	= Nothing /= borrowers (entries book)

numBorrowed :: Database -> Person -> Int 
numBorrowed db person = maybe 0 length (books db person)
--I think I finally understand how to use maybe

{-Not confident about below functions functioning. 
They definately aren't dealing with multiple persons having
the same book on loan. -}


makeLoan :: Database -> Person -> Book -> Database
makeLoan (DB entries) person book 
	= DB (\b -> if b==book then Just person else entries
	book)    

returnLoan :: Database -> Person -> Book -> Database
returnLoan (DB entries) person book 
	= DB (\b -> if b==book then Nothing else entries book)

	
