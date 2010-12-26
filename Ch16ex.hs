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

--valueF (updateF initialF "a" 5) "a" => Just 5
--valueF (updateF initialF "a" 5) "b" => Nothing

--16.4

{-Instead of an error provide a test that shows 
if a variable is in a given store. -}

testVal :: Store -> Var -> Bool 
testVal (Sto []) v = False
testVal (Sto ((n,vstored):sto)) v
	| v == vstored = True
	| otherwise = testVal (Sto sto) v

--16.5

--setAll should set every variable to the value n

--setAll :: Int -> Store

--It cannot be defined for Store as a list. 

--Defined for Store represent by a function:

setAllF :: Int -> StoreF
setAllF n = (StoF (\v -> Just n)) 

--valueF (setAllF 5) "a" => Just 5

--16.6

--Design a ADT for the library database
{-Made functions instead of lists to to understand 
lambda beter. It probably actually just make more sense to
have a list. Currently order to acess the inforamtion I
want I have to store it twice.  -} 

type Person = String
type Book = String
type Borrowers = [Person]
type Loans = [Book]

newtype DBByBook = DBBook (Book -> Borrowers)
newtype DBByBorrower = DBBorrower (Person -> Loans)
type Database = (DBByBook, DBByBorrower)

newDB :: Database
newDB = (DBBook (\b -> []), DBBorrower (\p -> []))

books :: Database -> Person -> Loans
books (bookDb, DBBorrower borrowerDb) p = borrowerDb p 

borrowers :: Database -> Book -> Borrowers 
borrowers (DBBook bookDb, borrowerDb) b = bookDb b

borrowed :: Database -> Book -> Bool 
borrowed (DBBook bookDb, borrowerDb) book	
	= (bookDb book) == []

numBorrowed :: Database -> Person -> Int 
numBorrowed db person = length (books db person)

makeLoan :: Database -> Person -> Book -> Database 
makeLoan (DBBook bookDB, DBBorrower borrowerDB) p b 
	= (DBBook (\book -> 
	if book==b 
	then (p : (bookDB book))
	else (bookDB book)),
	(DBBorrower (\person -> 
	if person==p 
	then b : (borrowerDB person)
	else (borrowerDB person))))

returnLoan :: Database -> Person -> Book -> Database 
returnLoan (DBBook bookDB, DBBorrower borrowerDB) p b 
	= (DBBook (\book -> 
	if book==b 
	then remove b (bookDB book)
	else (bookDB book)), 
	DBBorrower  (\person -> 
	if person==p 
	then remove p (borrowerDB person)
	else (borrowerDB person)))
	where 
		remove _ [] = []
		remove n (x:xs) 
			| n==x = xs
			| otherwise = remove n xs

--16.3

{-Calculation:

"abcde" ++ "f" = "abcdef"

init "abdcef" 
= take (length "abcdef" - 1) "abcde 
= take ( 1 + length "bcdef" - 1) "abcdef"
= take ( 1 + 1 + length "cdef" - 1) "abcdef"
= take ( 1 + 1 + 1 + length "def" - 1) "abcdef"
= take ( 1 + 1 + 1 + 1 + length "ef" - 1) "abcdef"
= take ( 1 + 1 + 1 + 1 + 1 + length "f" - 1) "abcdef"
= take ( 1 + 1 + 1 + 1 + 1 + 1 + 1 +  length "" - 1) "abcdef"
= take ( 1 + 1 + 1 + 1 + 1 + 1 + 1 + 0 - 1) "abcdef"
= take 5 "abcdef" 
= "a" : take 4 "abcde"  
= "a" : "b" : take 3 "cdef" 
= "a" : "b" : "c" : take 2 "edef" 
= "a" : "b" : "c" : "d" : take 1 "ef" 
= "a" : "b" : "c" : "e" : take 0 "f"
= "a" : "b" : "c" : "d" : "e" : []
= "abcde"

last "abcdef" 
= "abcdef" !! (length "abcdef" - 1) 
= "abcdef" !! (length "abcdef" - 1) 
= "abcdef" !! ( 1 + length "bcdef" - 1) 
= "abcdef" !! ( 1 + 1 + length "cdef" - 1) 
= "abcdef" !! ( 1 + 1 + 1 + length "def" - 1) 
= "abcdef" !! ( 1 + 1 + 1 + 1 + length "ef" - 1) 
= "abcdef" !! ( 1 + 1 + 1 + 1 + 1 + length "f" - 1) 
= "abcdef" !! ( 1 + 1 + 1 + 1 + 1 + 1 + 1 +  length "" - 1) 
= "abcdef" !! ( 1 + 1 + 1 + 1 + 1 + 1 + 1 + 0 - 1) 
= "abcdef" !! 5 
= "f"

-}

--16.8 

{- Example Operations: add2, add1, removeitem, add 3,
removeitem, add 1, add 4, removeitem, removeitem

Qu []
Qu [2]
Qu [2, 1]
(2,  Qu [1])
Qu [1, 3]
(1, Qu [3])
Qu [3, 1]
Qu [3, 1, 4]
(3, Qu [1, 4])
(1, Qu [4])

Qu []
Qu [2]
Qu [1, 2]
(2, Qu [1])
Qu [3, 1]
(1, Qu [3])
Qu [1, 3] 
Qu [3, 1, 4]
(3, Qu [1, 4]) 
(1, Qu [4])

Qu [] []
Qu [] [2]
Qu [] [1, 2] 
remQ (Qu reverse [2, 1] []) = (2, Qu [1] [])
Qu [1] [3]
(1, Qu [] [3])
Qu [] [1, 3] 
Qu [] [4, 1, 3]
remQ (Qu reverse [4, 1, 3] []) = (3, Qu [1, 4] [])
(1, Qu [4] []) 
-}

--16.9 

--module Deque ( Deque, emptyDQ, isEmptyDQ, addDQHead,addDQTail, remDQHead, remDQTail) 

--Deque Implementation 1: 

newtype Deque a = DQ [a] 

emptyDQ = DQ []

isEmptyDQ (DQ []) = True
isEmptyDQ _ = False

addDQHead x (DQ xs) = (DQ x:xs) 

addDQTail x (DQ xs) = (DQ (xs ++ [x]))

remDQHead dq@(DQ (x:xs))
	| not (isEmptyDQ dq) = (x, (DQ xs))
	| otherwise = error "remDQ"

remDQTail dq@(DQ xs) 
	| not (isEmptyDQ dq) = ((tail xs),(DQ (init xs))) 
	| otherwise = error  "remDQ"
	
--DeQue Implementation 2: 

data DequeMod a = DQM [a] [a] 

emptyDQM = (DQM [] [])

isEmptyDQM (DQM [] []) = True
isEmptyDQM _ = False

addDQMHead x (DQM ys xs) = (DQM ys (x:xs)) 

addDQMTail x (DQM xs ys) = (DQM xs (ys++[x]))

remDQMHead (DQM (x:xs) ys) = (x, (DQM xs ys))
remDQMHead (DQM [] ys) = remDQMHead (DQM (reverse ys) []) 
remDQMHead (DQM [] []) = error "remDQ"

remDQMTail (DQM xs ys) = (tail xs, (DQM xs (init ys)))
remDQMTail (DQM xs []) = (tail xs, (DQM (init xs) [])) 
remDQMTail (DQM [] []) = error "remDQ"

--16.10

newtype UniqueQueue a = UniQ [a]

emptyUniQ = UniQ []

isEmptyUniQ (UniQ []) = True
isEmptyUniQ _ = False

addUniQ x (UniQ xs) 
	| elem x xs = (UniQ xs) 
	| otherwise = (UniQ (xs++[x]))

remUniQ u@(UniQ (x:xs)) 
	| not (isEmptyUniQ u) = (x, (UniQ xs))
	| otherwise = error "remUniQ"

--16.11

newtype PriorityQueue a = PQ [(a,Int)]

emptyPQ = PQ []

isEmptyPQ (PQ []) = True
isEmptyPQ (PQ _) = False

addPQ x n (PQ []) = (PQ [(x,n)])
addPQ x n (PQ ys)
	| elem x (map fst ys) = (PQ ys) 
	| otherwise = (PQ (ys++[(x,n)]))

remPQ (PQ [(x,n)]) = (x, (PQ []))
remPQ (PQ ((y,n):ys))
	| n >= snd(ys !! 2) = remPQ (PQ ((y,n):(tail ys)))
	| n < snd(ys !! 2) = remPQ (PQ ys)

--16.12

{-Use Proiriety Queues for Huffman Encodeing?-} 

--16.13

{-Are all the operations from the module Tree nessecary? 

module Tree (Tree, nil, isNil, isNode, leftSub, rightSub,
treeVal, insTree, delete, minTree)

isNil could be defined in terms of nil

minTree could be defined by treeVal -}

--Am I missing anything?

--16.14
--Same as 16.6

--16.15


