--The Craft of Functional Programming 
--Ch. 9 Generalization: Patterns of Computation 

import Prelude hiding (length, last, init, lookup, takeWhile, getLine)
import Ch7ex hiding (and, drop, take, splitAt, reverse, take, dropWord, dropSpace, splitWords, getWord, getLine, dropLine)
import Ch5ex hiding (returnLoan)

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
--zipWith :: (a -> b_-> c) -> [a] -> [b] -> [c]

minList :: [Int] -> Int 
minList [x] = x
minList (x:xs)
	|x < minList xs = x
	|otherwise = minList xs 
	

minReturn :: (Int -> Int) -> Int -> Int 
minReturn f n = minList (map f [0..n])

minReturnA :: (Int -> Int) -> Int -> Int 
minReturnA f n = foldr1 minListA (map f [1..n])
	where 
	minListA :: Int -> Int -> Int
	minListA x y
		|x < y = x
		|otherwise = y 

{-write out calculation
minReturnA (mod 5) 4
foldr1 minListA (map (mod 5) [1, 2, 3, 4])
foldr1 minListA [1, 2, 3, 4, 5] 
foldr minListA [1] [2, 3, 4, 5]
minListA 2 (foldr minListA [1] [3, 4, 5])
minListA 2 (minListA 3 (foldr minListA [1] [4, 5]))
minListA 2 (minListA 3 (minListA 4 (foldr minListA [1] [5])))
minListA 2 (minListA 3 (minListA 4 (minListA 5(foldr minListA [1] [])))
minListA 2 (minListA 3 (minListA 4 (minListA 5 1)))
minListA 2 (minListA 3 (minListA 4 1))
minListA 2 (minListA 3 1) 
minListA 2 1
1-}

allEqual :: (Int -> Int) -> Int -> Bool
allEqual f n = and (map (==(f 0)) (map f [0..n]))

posReturn :: (Int -> Int) -> Int -> Bool 
--posReturn f n = and (map (>0) (map f [0..n]))	
posReturn f n = and (map ((>0).f) [0..n]) 	 

isSorted :: [Int] -> Bool 
isSorted xs = (xs == iSort xs)

isSortedAndPosReturn :: (Int -> Int) -> Int -> Bool 
isSortedAndPosReturn f n = (posReturn f n) && (isSorted (map f [0..n])) 

--9.8

twice :: (Int -> Int) -> Int -> Int
twice f n = f (f n)   

--9.9
 
iter :: Int -> (a -> a) -> a -> a
iter n f x 
	|n ==0 = x
    |n ==1 = f x 
	|n > 1 = iter (n-1) f (f x)      

--9.10

double = (2*)

twoToTheN :: Int -> Int 
twoToTheN n = iter (n-1) double 2 
 
 --9.11
 
--foldr1 :: (a -> a -> a) -> [a] -> a 
--foldr :: (a -> b -> b) -> b -> [a] -> b
 
sumSqs :: Int -> Int 
sumSqs n = foldr (+) 0 (map (2^) [1..n])

--9.12
 	
sumPosIntSq :: [Int] -> Int 
sumPosIntSq xs = foldr1 (+) (map (2^) (filter (>=0) xs)) 	

--9.13

unZip :: [(a,b)] -> ([a],[b])
unZip list = (foldr (:) [] (map frt list) , foldr (:) [] (map scd list)) 

--foldr f s [] = s
--foldr f s (x:xs) = f x (foldr s xs)

last :: [a] -> a
--last [x] = x
--last (x:xs) = last xs 

--last list = head (drop n list) 
--	where 
--	n = length list -1  

--last list = head (iter (length list-1) (drop 1) list)

--last list = head (reverse list) 

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x] 

--last list = head (foldr snoc [] list) 

--last list = foldr1 k list
--	where
--	k :: a -> a -> a 
--	k x y = y 
	
last list = foldr1 (\x y -> y) list
	
{---Write out Calculation 
last "happy"
foldr1 (\x y -> y) "happy"

the definition of foldr1 in the book is wrong!
-}
init :: [a] -> [a]
--init [x] = []
--init (x:xs) = x: init xs

--init list = frt(splitAt (length list - 1) list)

--init list = take (length list - 1) list

--init list = reverse (drop 1 (reverse list))

init list = foldr snoc [] (drop 1 (foldr snoc [] list))  

--9.14

mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs)
	where 
	sing x = [x]

--returns id

--9.15
 
formatLine :: Line -> String 
formatLine ln = (foldr (++) [] (map (++" ") (init ln))) ++ last ln  

formatLines :: [Line] -> String 
formatLines lns = foldr (++) [] (map (++"\n") (map formatLine lns)) 

--9.16

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs) 
	|p x = x : filterFirst p xs
	|otherwise = xs
	
returnLoan :: Database -> Person -> Book -> Database
--returnLoan dBase pers bk = [pair | pair <-dBase, pair /=(pers, bk)]
returnLoan dBase per bk = filterFirst ((per,bk)/=) dBase	

--9.17

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p list = reverse (filterFirst p (reverse list))

--How to do this w/o filterFirst?

--9.18
--Redo Supermarket Billing using higher order functions

type Name = String
type Price = Int
type BarCode = Int
type TillType = [BarCode]
type BillType = [(Name, Price)]
type DatabaseSL = [(BarCode, Name, Price)]

codeIndex :: DatabaseSL
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence price 
  |digits ==4 = frt(splitAt 2 (show price)) ++ "." ++ scd (splitAt 2 (show price))
  |digits ==3 = frt(splitAt 1 (show price)) ++ "." ++ scd (splitAt 1 (show price))
  |digits ==2 = "."++ (show price)
  |digits ==1 = ".0"++(show price)
  where 
  digits = length (show price)

formatLineBill :: (Name, Price) -> String
formatLineBill (name, price) = name ++ (foldr (++) [] (replicate n ".")) ++ (formatPence price) ++ "\n"
	where
	n=lineLength - (length name) - (length (formatPence price))  
  
example :: [(Name, Price)]
example =[("Fish Fingers", 1231), ("Nappies" , 1010),("Orange Jelly", 56)]

formatLinesBill :: [(Name, Price)] -> String
formatLinesBill list = foldr (++) [] (map formatLineBill list)

makeTotal :: BillType -> Price
makeTotal list = foldr (+) 0 (map scd list)

formatTotal :: Price -> String
formatTotal total= "\nTotal" ++ (foldr (++) [] (replicate n ".")) ++ (formatPence total)
 where
 n=lineLength - 5 - (length (formatPence total))

formatBill :: BillType -> String
formatBill list = "\tHaskell Store\n\n"++(formatLinesBill list)++(formatTotal(makeTotal list))

frt3 :: (a,b,c) -> a
frt3 (x,_,_) = x

scd3 :: (a,b,c) -> (b,c)
scd3 (_,x,y) = (x,y)

lookup :: BarCode -> (Name, Price)
lookup code
	|(filter findCode codeIndex)== [] = ("Unknown Item", 0)
	|otherwise = head(map scd3 (filter findCode codeIndex)) 
	where
	findCode :: (BarCode, Name, Price) -> Bool
	findCode x = code == frt3 x

exampleTill :: TillType
exampleTill = [1111, 1110, 1112]

makeBill :: TillType -> BillType
makeBill codeList = map lookup codeList 

example2 :: [(Name, Price)]
example2 =[("Fish Fingers", 1231), ("Dry Sherry, 1lt", 540), ("Nappies" , 1010),("Orange Jelly", 56), ("Dry Sherry, 1lt", 540)]

makeDiscount:: BillType -> Int
makeDiscount bill= n*100
  where 
  n= (length (filter (("Dry Sherry, 1lt", 540)==) bill)) `div` 2 
  
formatDiscount :: Price -> String
formatDiscount discount= "\nDiscount" ++ (foldr (++) [] (replicate n "."))++(formatPence discount)++"\n"
 where
 n=lineLength - 8 - (length (formatPence discount))

formatBillBetter :: BillType -> String
formatBillBetter list = "\tHaskell Store\n\n"++(formatLinesBill list)++(formatDiscount(makeDiscount list))++(formatTotal((makeTotal list) - (makeDiscount list)))

removeCode :: BarCode -> DatabaseSL -> DatabaseSL
removeCode code dBase = filter delete dBase
	where
	delete x = code/= frt3 x 

exnew1::(BarCode, Name, Price)
exnew1= (4719, "Balls" , 221)

exnew2::(BarCode, Name, Price)
exnew2= (4712, "Bigger Balls" , 2281)

newCode :: (BarCode, Name, Price) -> DatabaseSL
newCode index = index : (removeCode (frt3 index) codeIndex)

formatLinesBillmod :: [(Name, Price)] -> String
formatLinesBillmod list = foldr (++) [] (map formatLineBill (filter (("Unknown Item", 0)/=) list))

formatBillmod :: BillType -> String
formatBillmod list = "\tHaskell Store\n\n"++(formatLinesBillmod list)++(formatTotal(makeTotal list))

--9.19

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
	|p x = dropUntil p xs
	|otherwise = (x:xs)

notWhiteSpace :: Char -> Bool 
notWhiteSpace chr = not (elem chr whitespace) 

dropWord :: String -> String 
dropWord str = dropUntil notWhiteSpace str

--9.20

isWhiteSpace :: Char -> Bool 
isWhiteSpace chr = elem chr whitespace
 
dropSpace :: String -> String
dropSpace str = dropUntil isWhiteSpace str 

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p [] = []
getUntil p (x:xs)
	| p x = []
	|otherwise = x: getUntil p xs
	
{-
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
	|p x = x: takeWhile p xs
	|otherwise = []
-}

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p list = getUntil f list
	where
--	f :: a -> Bool Rigid Types?
	f e = not (p e)

--9.22

getWord :: String -> Word
getWord xs = getUntil p xs 
	where 
	p x = elem x whitespace

splitWords :: String -> [Word]
splitWords [] = [] 
splitWords st = (getWord st): splitWords (dropSpace (dropWord st))

{-{-Old defintion 
getLine :: Int -> [Word]-> Line
getLine len [] = []
getLine len (w:ws)
  |length w <=len = w: restOfLine
  |otherwise = []
  where
  newlen = len - (length w + 1)
  restOfLine = getLine newlen ws
-}
--Want to redefine getLine with getUntil, but having trouble defining spaceLeftInLine. How do I get the list of wordsUsed?  
getLine :: [Word]-> Line
getLine wds@(w:ws) = getUntil (<=spaceLeftInLine) wds
	where
	spaceLeftInLine = lineLen - spaceUsed
	spaceUsed = charsUsed + spacesUsed
	charsUsed = sum (map length wordsUsed) 
	spacesUsed = length wordsUsed

{-Old defintion    
dropLine :: Int ->  [Word] -> Line
dropLine _ [] = []
dropLine len (w:ws)
  |length w <=len = restOfLines
  |otherwise = (w:ws)
  where 
  newlen = len - (length w +1)
  restOfLines = dropLine newlen ws 
-}
--same issues as attempt to redine getLine
dropLine :: [Word] -> Line
dropLine wds@(w:ws) = dropUntil (w <=spaceLeftInLine) wds
	where
	spaceLeftInLine = lineLen - spaceUsed
	spaceUsed = charsUsed + spacesUsed
	charsUsed = sum (map length wordsUsed) 
	spacesUsed = length wordsUsed

--test values
lineLen:: Int
lineLen = 15  

ln ::[Word]
ln = ["1", "234", "567", "8901234", "56"]

someWords :: [Word]
someWords =["I", "am", "not", "a", "very", "good", "typer", "but", "I", "need", "to", "type", "a", "lot", "to", "test", "my", "haskell", "program"]
  
splitLines :: [Word] -> [Line] 
splitLines [] = []
splitLines ws = getLine ws : splitLines (dropLine ws) 	-}


--9.22

--getLine from Ch7ex is polymorphic it was orginally typed as 
--getLine :: Int -> [Word]-> Line
--is can be typed most broadly as
--getLine :: Int -> [[a]] -> [[a]]
getLine len [] = []
getLine len (w:ws)
  |length w <=len = w: restOfLine
  |otherwise = []
  where
  newlen = len - (length w + 1)
  restOfLine = getLine newlen ws

--exampleOfTest 5 [[1], [1,2,3], [12,32,12], [2]]

--9.23

--generalization of types

--dropLine :: [Word] -> Line
--dropLine :: [[a]] -> [[a]]

--splitLines :: [Word] -> [Line] 
--splitLines :: [[a]] -> [[[a]]]			