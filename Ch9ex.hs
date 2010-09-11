--The Craft of Functional Programming 
--Ch. 9 Generalization: Patterns of Computation 

import Prelude hiding (length, last, init, lookup)
import Ch7ex hiding (and, drop, take, splitAt, reverse, take)
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
		--(Int -> [a]-> [a]) -> Int -> [a] -> 
 
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

last list = head (foldr snoc [] list) 
--Goal of exercise was to use foldr. Is there a more direct way to do this?

init :: [a] -> [a]
--init [x] = []
--init (x:xs) = x: init xs

--init list = frt(splitAt (length list - 1) list)

init list = take (length list - 1) list

--Also, goal is to write w/foldr. I don't see this, just lots of other ways. 

--9.14

mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs)
	where 
	sing x = [x]

--returns id

--9.15

--formatList :: (a -> String) -> [a -> String 
--The book wants me to make a program as typed above and use it to make formatLines. I cannot figure out the point of this function. 

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

{-Still needs to be rewritten

lookmod:: DatabaseSL -> BarCode -> [(Name, Price)]
lookmod dBase findCode = [(name, price) | (code, name, price) <-dBase, code==findCode]

makeBillmodHelper:: TillType -> Int -> BillType
makeBillmodHelper codelist (-1) =[]
makeBillmodHelper codelist n= (makeBillmodHelper codelist (n-1)) ++ (lookmod codeIndex (codelist!!n))

makeBillmod :: TillType -> BillType
makeBillmod codeList = makeBillmodHelper codeList ((length codeList)-1)
-} 