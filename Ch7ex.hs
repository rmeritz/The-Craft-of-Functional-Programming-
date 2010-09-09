--The Craft of Functional Programming 
--Ch. 7 Defining Functions Over Lists

import Prelude hiding (product, and, or, reverse, unzip, drop, take, zip3,getLine, splitAt)

--7.1

frtintplusone :: [Int] -> Int 
frtintplusone [] = 0
frtintplusone (x:xs) = x + 1

--7.2

frsttwo:: [Int] -> Int
frsttwo [] = 0
frsttwo [x] = x
frsttwo(x:y:xs) = x + y

--7.3

nomatchfrtintplusone :: [Int] -> Int
nomatchfrtintplusone list
  |length(list) > 0 = head list
  |otherwise =0
  
nomatchfrsttwo:: [Int] -> Int
nomatchfrsttwo list
  |length(list) > 1 = head list + head (tail list)
  |length(list) == 1 = head list
  |otherwise =0

--7.4

product :: [Int] -> Int 
product [] = 1
product (x:xs) = x*(product xs) 

--7.5

and,or :: [Bool] -> Bool
and []= True 
and (x:xs) = x && (and xs)
or []=False
or (x:xs) = x || (or xs)

--7.6

elemNum :: Int -> [Int] -> Int
elemNum y [] = 0
elemNum y (x:xs)
  |y==x = 1 + (elemNum y xs)
  |otherwise = elemNum y xs

nomatchelemNum :: Int -> [Int] -> Int
nomatchelemNum y list = length [x | x <-list, id x == y]

--7.7

nomatchunique :: [Int] -> [Int]
nomatchunique list = [x | x <- list, elemNum x list==1]

remove :: Int -> [Int] -> [Int]
remove y [] = []
remove y (x:xs) 
  |y/=x = x:(remove y xs)
  |otherwise = remove y xs
  
unique :: [Int] -> [Int] 
unique [] = []
unique (x:xs)
  |elemNum x xs ==0 = x:(unique xs)
  |otherwise = unique (remove x xs)
  
--7.8

reverse :: [a] -> [a]
reverse []=[]
reverse (x:xs) = reverse xs ++ [x]

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y):ps) = (x:xs,y:ys)
  where (xs,ys)=unzip ps  

--7.9

iSort :: [Int] -> [Int]
iSort []     = [] 
iSort (x:xs) = ins x (iSort xs) 

ins :: Int -> [Int] -> [Int]
ins x []    = [x] 
ins x (y:ys) 
  | x <= y      = x:(y:ys)
  | otherwise   = y : ins x ys

maxlist, minlist :: [Int] -> Int
maxlist x = head (reverse (iSort x))
minlist x = head (iSort x)

maxlistmod :: [Int] -> Int 
maxlistmod [x] = x
maxlistmod (x:xs) 
  |x> maxlistmod xs = x
  |otherwise = maxlistmod xs
  
minlistmod :: [Int] -> Int 
minlistmod [x] = x
minlistmod (x:xs) 
  |x< minlistmod xs = x
  |otherwise = minlistmod xs
  
--7.11

insDescend :: Int -> [Int] -> [Int]
insDescend x []    = [x] 
insDescend x (y:ys) 
 | x >= y         = x:(y:ys)
 | otherwise      = y : insDescend x ys
  
iSortDescend :: [Int] -> [Int]
iSortDescend []     = [] 
iSortDescend (x:xs) = insDescend x (iSortDescend xs) 

insRemove :: Int -> [Int] -> [Int]
insRemove x []    = [x] 
insRemove x (y:ys) 
  | x < y      = x:(y:ys)
  | x == y     = (y:ys)
  | otherwise  = y : insRemove x ys

iSortNoDuplicate :: [Int] -> [Int]
iSortNoDuplicate  []     = [] 
iSortNoDuplicate  (x:xs) = insRemove x (iSortNoDuplicate  xs)

--7.13

insPair :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insPair (x,y) [] = [(x,y)]
insPair (x,y) ((z,w):ps)
 |x < z = (x,y):(z,w):ps
 |(x == z) && (y < w ) = (x,y):(z,w):ps
 |otherwise = (z,w): insPair (x,y) ps

pairISort :: [(Int, Int)] -> [(Int, Int)]
pairISort []     = [] 
pairISort (p:ps) = insPair p (pairISort ps)

--7.14

drop :: Int -> [a] -> [a]
drop _ [] = [] 
drop n (x:xs)
  |n>0 = drop (n-1) xs
  |n==0 =(x:xs)
  |n<0 = error "PreludeList.drop: negative argument"
  
splitAt ::Int -> [a] -> ([a],[a])
splitAt _ [] = ([],[])
splitAt 0 ps = ([], ps)
splitAt n (x:xs) = ((x:rest), end)
  where
  (rest, end) = splitAt (n-1) xs
  
--7.15

take :: Int -> [a] -> [a]
take 0 _ = []  
take n [] 
  |(n>=0) = []
take n (x:xs)  
  |n >= 0 = x:take(n-1) xs
  |n<0 = error "Prelude.list take: negative argument"
take n [] 
  |otherwise = error "Prelude.list take: negative argument"

--7.16

zip3:: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z):zip3 xs ys zs
zip3 _ _ _ = []


nomatchzip3 ::[a] -> [b] -> [c] -> [(a,b,c)]
nomatchzip3 x y z = format( zip x (zip y z))
  where
  format [] = []
  format ((a,(b,c)):ps) = (a,b,c): format ps

--7.17

qSort :: [Int]-> [Int]
qSort[] = []
qSort (x:xs) = qSort [y | y <-xs, y<=x] ++ [x] ++ qSort[y |y <-xs, y>x]

qSortDescend :: [Int] -> [Int]
qSortDescend[] = []
qSortDescend (x:xs) = qSortDescend [y | y <-xs, y>=x] ++ [x] ++ qSortDescend [y |y <-xs, y<x]

qSortNoDuplicate :: [Int]-> [Int]
qSortNoDuplicate[] = []
qSortNoDuplicate (x:xs) = qSortNoDuplicate [y | y <-xs, y<x] ++ [x] ++ qSortNoDuplicate[y |y <-xs, y>x]

--7.18

--same as built-it function elem
isElementOf :: (Eq a)=> a -> [a] -> Bool
isElementOf _ []     = False
isElementOf e (x:xs) =  e == x || e `isElementOf` xs

sublist :: (Eq a)=> [a] -> [a] -> Bool 
sublist [] ys = True
sublist (x:xs) ys = (isElementOf x ys) && (sublist xs ys)

whichElement :: (Eq a)=> a ->[a] -> Int -> Int  
whichElement _ [] _ = 0
whichElement e (x:xs) n
  |e==x = n
  |otherwise = whichElement e xs (n+1)


subsequenceHelper [] _ = True 
subsequenceHelper _ [] = False
subsequenceHelper (x:xs) (y:ys) = (x==y) && (subsequenceHelper xs ys)

 
subsequence _ []= False
subsequence xs ps@(y:ys) = subsequenceHelper xs ps || subsequence xs ys 
  
--7.19

type Word = String 
type Line = [Word]

whitespace = ['\n', '\t',' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  |isElementOf x whitespace = []
  |otherwise = x: getWord xs

dropWord :: String -> String 
dropWord [] = []
dropWord (x:xs)
  |isElementOf x whitespace = (x:xs)
  |otherwise = dropWord xs
  
dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  |isElementOf x whitespace = dropSpace xs
  |otherwise = (x:xs)

split :: String -> [Word]
split [] = [] 
split st = (getWord st): split (dropSpace (dropWord st))
  
splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

getLine :: Int -> [Word]-> Line
getLine len [] = []
getLine len (w:ws)
  |length w <=len = w: restOfLine
  |otherwise = []
  where
  newlen = len - (length w + 1)
  restOfLine = getLine newlen ws
  
dropLine :: Int ->  [Word] -> Line
dropLine _ [] = []
dropLine len (w:ws)
  |length w <=len = restOfLines
  |otherwise = (w:ws)
  where 
  newlen = len - (length w +1)
  restOfLines = dropLine newlen ws 

--test values
lineLen:: Int
lineLen = 15  

ln ::[Word]
ln = ["1", "234", "567", "8901234", "56"]

someWords :: [Word]
someWords =["I", "am", "not", "a", "very", "good", "typer", "but", "I", "need", "to", "type", "a", "lot", "to", "test", "my", "haskell", "program"]
  
splitLines :: [Word] -> [Line] 
splitLines [] = []
splitLines ws = getLine lineLen ws : splitLines (dropLine lineLen ws)  

--7.20

joinLine :: Line -> String 
joinLine []  = ""
joinLine [w] = w
joinLine (w:ws)=  w ++ " " ++joinLine ws
 
--7.21

--same as function intersperse 
intercalate :: String ->  [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate joiner (x:xs) = x ++ joiner ++ (intercalate joiner xs) 

joinLines :: [Line] -> String
joinLines [] = ""
joinLines (l:ls) = joinLine l ++"\n"++ joinLines ls

--7.22
--Redine functions so they work like splitAt instead of take and drop but I need to unserstand splitAt first

--7.23

joinSpaces :: [String] -> String
joinSpaces [] = ""
joinSpaces (x:xs) = x++ joinSpaces xs 

joinLineJustifyHelper :: Int -> Int -> Int -> Int -> Line -> String
joinLineJustifyHelper e n ee nw (w:ws)
 |nw==1 = w  
 |(ee>= 1)= w ++ (joinSpaces (replicate ss " "))++ joinLineJustifyHelper e n (ee-1) (nw-1) ws
 |otherwise  = w ++ (joinSpaces (replicate s " "))++ joinLineJustifyHelper e n 0 (nw-1) ws 
  where
  s = (e `div` n)   
  ss = s+1

charsInLine :: Line -> Int 
charsInLine [] = 0 
charsInLine (x:xs) = length x + charsInLine xs   

joinLineJustify :: Line -> String
joinLineJustify lns = joinLineJustifyHelper whiteSpacesTotal numberOfGaps extraWhiteSpaces numberOfWords lns 
  where 
  whiteSpacesTotal= lineLen - (charsInLine lns)
  numberOfGaps= numberOfWords - 1 
  extraWhiteSpaces = whiteSpacesTotal `mod` numberOfGaps 
  numberOfWords = length lns 

joinLinesJustify :: [Line] -> String
joinLinesJustify (l:ls)
  |length (l:ls)> 1 = joinLineJustify l ++"\n"++ joinLinesJustify ls
  |otherwise = joinLine l 

--7.24
numberOfChars ::String -> Int ->  Int
numberOfChars "" n = n 
numberOfChars (c:cs) n 
  |elem c whitespace == False  =numberOfChars cs n+1
  |otherwise = numberOfChars cs n  

numberOfWords :: String -> Int 
numberOfWords str = length (split str)

numberOfLines:: String -> Int -> Int 
numberOfLines "" n = n 
numberOfLines (c:cs) n
  |c=='\n' =numberOfLines cs (n+1)
  |otherwise =numberOfLines cs n  

wc :: String -> (Int, Int, Int)
wc str = (numberOfChars str 0, numberOfWords str, numberOfLines str 0)

numberOfLinesFormat :: String -> Int
numberOfLinesFormat str = length (splitLines(splitWords str))

wcFormat :: String -> (Int, Int, Int)
wcFormat str = (numberOfChars str 0, numberOfWords str, numberOfLinesFormat str)

--7.25
charctures = ['!','-', ':', ';', '.', '"', '`', ',', '?', '\'']

ord :: Char -> Int
ord  =  fromEnum

chr  :: Int  -> Char
chr  =  toEnum

offset :: Int
offset = ord 'A' - ord 'a'

allLower :: String -> String
allLower "" = "" 
allLower (c:ch)
  |elem c charctures || elem c whitespace  = allLower ch 
  |ord c <= 90 = (chr(ord c - offset)): allLower ch 
  |otherwise = c: allLower ch 

isPalinHelper :: String -> Int ->  Bool 
isPalinHelper "" _ = True
isPalinHelper _ 1 = True
isPalinHelper (c:cs) l = (c == ((c:cs)!!(l-1))) && (isPalinHelper (take (l-2) cs) (l-2))  

isPalin :: String -> Bool 
isPalin str = isPalinHelper newstr l 
  where
  newstr = allLower str
  l= length newstr

--7.26
 
--Example data
st, oldSub, newSub :: String 
st= "How tall is that?"
oldSub = "tall is"
newSub = "much toothpaste"

subsitute :: [Word] -> Word -> Word -> [Word]
subsitute [] _ _ = []
subsitute (c:cs) o n 
  |(c==o) = n :subsitute cs o n 
  |otherwise = c: subsitute cs o n  

subWord :: String -> String -> String -> String 
subWord string old new = joinLine(subsitute wordlist old new)  
  where  
  wordlist= splitWords string 

subsequenceLocation :: String -> String -> Int -> [Int]
subsequenceLocation _ "" _ = []
subsequenceLocation old (s:ss) n 
  |subsequenceHelper old (s:ss) = n: subsequenceLocation old ss (n+1) 
  |otherwise = subsequenceLocation old ss (n+1) 

frt (x,y) = x
scd(x,y)=y

subSt :: String -> String -> String -> String
subSt "" _ _ = ""
subSt st o n  
  |length locate > 0 = beginning ++ n ++ (subSt restOfList o n)
  |otherwise = st
  where 
  locate = subsequenceLocation o st 1
  stLen = length o 
  beginning = frt(splitAt (head locate) st) 
  end = scd (splitAt (head locate) st)
  restOfList = drop stLen end
  
