--The Craft of Functional Programming 
--Ch. 10 Functions as Values

import Prelude hiding (succ, flip, not, lines)
import Pictures

--10.1

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

--printBill :: TillType -> IO ()
--printBill = putStr . formatBill . makeBill
--printBill = makeBill >.> formatBill >.> putStr

--10.2

--(f . id) returns the value of f x

--(id . f) retuns the value of f x  

--f id  is same as (f . id)

--if f::(Int -> Bool)
--(f . id); id :: Int -> Int
--(id . f); id :: Bool -> Bool 
--f id ; id :: Int -> Int

--10.3

composeList :: [(a -> a)] -> (a -> a)
composeList listOfFunctions = foldr (>.>) id listOfFunctions 

--10.4

succ :: Int -> Int
succ n = n+1

iter :: Int -> (a -> a) -> (a -> a)
iter n f 
  | n>0         = f . iter (n-1) f
  | otherwise   = id

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

{-Calculations
iter 3 double 1
double . iter 2 double 1 
double . double . double . iter 1 double 1
double . double . double . id 1 
2* . 2* . 2* . 1
2* . 2* . 2 
2* . 4
8 

(comp2 succ (*)) 3 4
* (succ3) (succ4)
* (3+1) (4+1)
* 4 5
20

comp2 sq add 3 4
add (sq 3) (sq 4)
add (3*3) (4*4)
add 9 16
25
-}

--10.5

tenFive :: Int -> (Int -> Int)  
tenFive = (\n -> iter n succ)

--10.6
{-
f :: (a -> b -> c)
\x y -> f x y  

g :: (b -> a -> c)
\x y -> f y x 
-}

--10.7 

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\x y -> f y x)
 
--10.8

not :: Bool -> Bool 
not = (\x ->
	case x of 
		True -> False
		False -> True)
{-
isWhiteSpace :: Char -> Bool
isWhiteSpace x 
	|\" " -> True
	|\"\n" -> True
	|\"\t" -> True
	|otherwise = False
-}	
--use case syntax

--10.9

total :: (Int -> Int) -> (Int -> Int)
total f = (\x -> sum (map f [0..x]))

--10.10

--Works but not as expected? 
slope :: (Float -> Float) -> (Float -> Float)
slope f = (\x -> (((f (x + 0.0001)) - (f x)) / 0.0001))
	
--10.11

integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f = (\upperbound lowerbound -> (((f upperbound) + (f lowerbound))/ 2)*(upperbound - lowerbound))

--10.12

--comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
--comp2 f g = (\x y -> g (f x) (f y))

comp2PA f g x  = (g (f x)).f    

totalPA f = sum . map f . listFrom0
	where
	listFrom0 :: Int -> [Int]
	listFrom0 n = [0..n]
	
--10.13

--(filter (>0) . map (+1)) is the same as (map (+1) . filter (>=0))

--10.14

isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)

black :: Picture
black = superimpose horse (invertColour horse)

chessBoard :: Int -> Picture
chessBoard n = foldr1 above (allRows n n)

allRows :: Int -> Int -> [Picture]
allRows 0 _ = []
allRows n y = alternateRow n y : allRows (n-1) y
	
alternateRow :: Int -> Int -> Picture
alternateRow n y
	|isEven n = row (rowA y)
	|otherwise = row (rowB y)
		 
row :: [Picture] -> Picture
row list = foldr1 sideBySide list
	
rowA :: Int -> [Picture]
rowA 0 = []
rowA n = alternateColorA n : rowA (n-1)
		
alternateColorA :: Int -> Picture
alternateColorA n 
	|isEven n = white 
	|otherwise = black 

rowB :: Int -> [Picture]
rowB 0 = []
rowB n = alternateColorB n : rowB (n-1)

alternateColorB :: Int -> Picture
alternateColorB n 
	|isEven n = black 
	|otherwise = white 
	
--10.15

type PictureB = [[Bool]]

exPB :: PictureB 
exPB = [[True, False], [False, False]]

exPB2 :: PictureB 
exPB2 = [[True, False], [True, False]]

invertColourB :: PictureB -> PictureB
invertColourB = map (map invertB)

invertB :: Bool -> Bool
invertB b = not b 

superimposeB :: PictureB -> PictureB -> PictureB
superimposeB = zipWith (zipWith combineB)

combineB :: Bool -> Bool -> Bool
combineB topB bottomB = topB && bottomB 

joinLineB :: [Bool] -> String 
joinLineB []  = ""
joinLineB [w] = show w
joinLineB (w:ws)=  (show w) ++ " " ++joinLineB ws
 
joinLinesB :: PictureB -> String
joinLinesB [] = ""
joinLinesB (l:ls) = joinLineB l ++"\n"++ joinLinesB ls

printPictureB :: PictureB-> IO ()
printPictureB = putStr . joinLinesB

--10.16
--foldr :: (a -> b -> b) -> b -> [a] -> b

makePicture :: Int -> Int -> [(Int, Int)] -> Picture
makePicture height width listofblackpts = foldr blackPoint (whitePicture height width) listofblackpts 

whitePicture :: Int -> Int -> Picture
whitePicture h w = replicate (h+1) (foldr (++) [] (replicate (w+1) ".")) 

blackPoint :: (Int, Int) -> Picture -> Picture
blackPoint (x,y) pic = (headRows 0 y pic)  ++ newRow ++ (restOfRows 0 y pic)
	where
	newRow = [(changePointInRow 0 x (selectRow 0 y pic))]

headRows :: Int -> Int -> Picture -> Picture
headRows n y (r:rs) 
	|n<y = r: headRows (n+1) y rs 
	|otherwise = []
	
restOfRows :: Int -> Int -> Picture -> Picture
restOfRows n y (r:rs) 
	|n < y = restOfRows (n+1) y rs     
	|otherwise = rs	 
	 
changePointInRow :: Int ->  Int -> String -> String
changePointInRow n x (p:ps)
	|n==x = "#"++ps
	|otherwise = p : changePointInRow (n+1) x ps  

selectRow :: Int -> Int -> Picture -> String
selectRow n y (r:rs)
	|n==y =r 
	|otherwise = selectRow (n+1) y rs 

--10.17

pictureToRep :: Picture -> (Int,  Int,  [(Int, Int)])
pictureToRep pic = (height pic, width pic, makePoints 0 (whereInRows pic))

width :: Picture -> Int 
width pic = length (head pic) -1

height :: Picture -> Int 
height pic = length pic -1

makePoints :: Int -> [[Int]] -> [(Int,Int)]
makePoints _ [] = []
makePoints n (p:ps) = map (makeTuple n) p ++ makePoints (n+1) ps

makeTuple :: Int -> Int -> (Int,Int)
makeTuple n x = (x,n)

whereInRows :: Picture -> [[Int]] 
whereInRows pic = map (whereInRow 0) pic
 
whereInRow :: Int -> String -> [Int]
whereInRow _ [] = []
whereInRow n (p:ps)
	|p =='#' = n : whereInRow (n+1) ps 
	|otherwise = whereInRow (n+1) ps  
	
--10.18

type Rep = (Int, Int, [(Int,Int)])

--Why is the better and worse than [[Char]]	?
--Data storage.
--Harder to visualize. 

rotateRep :: Rep -> Rep 
rotateRep = flipHRep.flipVRep 

flipHRep :: Rep -> Rep
flipHRep (h, w, points) = (h, w, map (flipHPoint h) points)

flipHPoint :: Int ->  (Int,Int) -> (Int, Int)
flipHPoint h (x,y) = (x, h-y)

flipVRep :: Rep -> Rep  
flipVRep (h, w, points) = (h, w, map (flipVPoint w) points)

flipVPoint :: Int ->  (Int,Int) -> (Int, Int)
flipVPoint w (x,y) = (w-x, y)

superimposeRep :: Rep -> Rep -> Rep 
superimposeRep (h, w, points) (h', w', points') = (h, w, superPoints (points++points'))

insPair :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insPair (x,y) [] = [(x,y)]
insPair (x,y) ((z,w):ps)
 |x < z = (x,y):(z,w):ps
 |(x == z) && (y < w ) = (x,y):(z,w):ps
 |otherwise = (z,w): insPair (x,y) ps

pairISort :: [(Int, Int)] -> [(Int, Int)]
pairISort []     = [] 
pairISort (p:ps) = insPair p (pairISort ps)

superPoints :: [(Int, Int)] -> [(Int, Int)]
superPoints = dropDouble. pairISort

dropDouble :: [(Int, Int)] -> [(Int, Int)]
dropDouble [] = []
dropDouble [x] = [x]
dropDouble (x:x':xs)
	|x==x' = dropDouble xs
	|otherwise = x : dropDouble (x':xs)
--could I filter somehow?
--filter :: (a -> bool) -> [a] -> [a] 
--filter ((head list)/=) list ...
	
rotate90Rep :: Rep -> Rep 
rotate90Rep (h,w, points) = flipVRep (w,h, map rotate90Point points)

rotate90Point :: (Int, Int) -> (Int, Int)
rotate90Point (x,y) = (y,x) 
	
--10.19
          
--Redo 6.2 exercises 

type Position = (Int, Int)
type Img = (Rep, Position)

makeImg :: Rep -> Position -> Img
makeImg rep pos = (rep, pos)

horseImg :: Img
horseImg = makeImg (pictureToRep horse) (13,13) 

frt (x,_)=x
scd (_,y)=y

changePositionImg :: Img -> Position -> Img 
changePositionImg img pos = (frt img, pos)

makePosition :: Int -> Int -> Position 
makePosition a b = (a, b)

moveImg :: Img -> Int -> Int -> Img
moveImg img moveX moveY = changePositionImg img ((frt(scd img)+ moveX), (scd(scd img) + moveY))

place :: Img -> Img
place ((h, w, points), (x,y)) = ((h+y, w+x, map (shiftPointsX x) points), (0,0))

shiftPointsX :: Int -> (Int, Int) -> (Int, Int)
shiftPointsX w (x,y) = (x+w, y)

showPicture :: Rep -> Picture
showPicture (height, width, listofblackpts) = foldr blackPoint (whitePicture height width) listofblackpts 

printImg :: Img -> IO()
printImg = printPicture. showPicture. frt. place 

flipHImgN :: Img -> Img 
flipHImgN img = ((flipHRep.frt) img, scd img)

flipVImgN :: Img -> Img 
flipVImgN img = ((flipVRep.frt) img, scd img)

rotateImgN :: Img -> Img
rotateImgN img = ((rotateRep.frt) img, scd img)

rotate90ImgN :: Img -> Img 
rotate90ImgN img = ((rotate90Rep.frt) img, (rotate90Point.scd) img)

flipHImg :: Img -> Img 
flipHImg img@((h,w,point),(x,y)) = moveImg (flipHImgN img) 0 (-h) 

flipVImg :: Img -> Img 
flipVImg img@((h,w,point),(x,y)) = moveImg (flipVImgN img) (-w) 0 

rotateImg :: Img -> Img
rotateImg img@((h,w,point),(x,y)) = moveImg (rotateImgN img) (-w) (-h) 

rotate90Img :: Img -> Img
rotate90Img img@((h,w,point),(x,y)) = moveImg (rotate90ImgN img) (-h) (-w)

superimposeImg :: Img -> Img -> Img  
superimposeImg img1 img2 = ((superimposeRep ((frt.place) img1) ((frt.place) img2)), (0,0))

--10.20

type Doc  = String
type Line = String
type Word = String

docEx :: Doc
docEx = "catherdral doggerel catherdral\nbattery doggerel catherdral\ncatherdral" 

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p [] = []
getUntil p (x:xs)
	| p x = []
	|otherwise = x: getUntil p xs
	
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs)
	|p x = dropUntil p xs
	|otherwise = (x:xs)

getL :: Doc -> Line 
getL = getUntil (=='\n')

dropL :: Doc -> Doc 
dropL = (dropUntil (=='\n')).(dropUntil (/='\n'))

lines :: Doc -> [Line]
lines "" = []
lines doc = getL doc : (lines.dropL) doc   

--10.21

exIntWord :: [(Int, Word)]
exIntWord = [(2,"bat"), (1,"cat"), (3, "cat"), (1, "doggy"), (2, "doggy")]

makeLists :: [(Int, Word)] -> [([Int],Word)]
--makeLists = map (\(n, st) -> ([n],st)) 
makeLists l = [([n],st)| (n,st) <- l]  

shorten :: [([Int],Word)] -> [([Int],Word)]
--shorten = filter (\(nl, wd) -> (length wd > 3))
shorten l = [(nl, wd)|(nl, wd) <- l, (length wd > 3)]

--10.22

makeIndex :: Doc -> [ ([Int],Word) ]
makeIndex
  = lines       >.>     --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]  

numLines :: [Line] -> [ ( Int , Line ) ]
numLines linels = zip [1 .. length linels] linels

numWords :: ( Int , Line ) -> [ ( Int , Word ) ]
numWords (number , line) = [ (number , word) | word <- splitWords line ]

whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

splitWords :: String -> [Word]
splitWords [] = [] 
splitWords st = (getWord st): splitWords (dropSpace (dropWord st))

getWord :: String -> Word
getWord xs = getUntil p xs 
	where 
	p x = elem x whitespace
	
notWhiteSpace :: Char -> Bool 
notWhiteSpace chr = not (elem chr whitespace) 

dropWord :: String -> String 
dropWord str = dropUntil notWhiteSpace str

isWhiteSpace :: Char -> Bool 
isWhiteSpace chr = elem chr whitespace
 
dropSpace :: String -> String
dropSpace str = dropUntil isWhiteSpace str 	

allNumWords :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]
allNumWords = concat . map numWords

orderPair :: ( Int , Word ) -> ( Int , Word ) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 )
  = w1 < w2 || ( w1 == w2 && n1 < n2 )

sortLs :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]
sortLs []     = []
sortLs (p:ps)
  = sortLs smaller ++ [p] ++ sortLs larger
    where
    smaller = [ q | q<-ps , orderPair q p ]
    larger  = [ q | q<-ps , orderPair p q ]
    
amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)    
 
type Pages = String  
  
makeIndexRange :: Doc -> [(Pages, Word)]  
makeIndexRange = makeIndex >.> (map ranger)

ranger :: ([Int],Word) -> (Pages, Word) 
ranger l = ((((intercalate ",").makeRange.frt) l), scd l)   

intercalate :: String ->  [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate joiner (x:xs) = x ++ joiner ++ (intercalate joiner xs)  

makeRange :: [Int] -> [Pages] 
makeRange [] = []
makeRange [n] = [show n] 
makeRange (n:n':ns) 
	|n' == n+1 = (show n ++"-"++ show n') : makeRange ns 
	|otherwise = show n : makeRange (n':ns) 
	
{-I need to fix makeRange so it can handle ranges longer an 2 
--[1,2,3,5,9,10]
--["1-3","5","9-10"]
makeRange [] = []
makeRange [n] = [show n] 	
makeRange (n:ns) 
	|n == startOfRange = (show n ++ "-" ++ show endOfRange) : makeRange (dropUntil endOfRange ns))
	|otherwise = show n : makeRange ns 
	where 
	startOfRange = 
	endOfRange = -}
	
--10.23

unSortedEx :: [ ( Int , Word ) ] 
unSortedEx = [(4, "cat"), (2,"bat"), (1,"doggy"), (3, "cat"), (1, "doggy"), (2, "doggy")]

orderPairMod :: ( Int , Word ) -> ( Int , Word ) -> Bool
orderPairMod ( n1 , w1 ) ( n2 , w2 )
  = w1 < w2 || ( w1 == w2 && n1 < n2 ) || ( w1 == w2 && n1 == n2 ) 

sortLsMod :: [ ( Int , Word ) ] -> [ ( Int , Word ) ]
sortLsMod []     = []
sortLsMod (p:ps)
  = sortLsMod smaller ++ [p] ++ sortLsMod larger
    where
    smaller = [ q | q<-ps , orderPairMod q p ]
    larger  = [ q | q<-ps , orderPair p q ]

--10.24

{-Write using getUntil and dropUntil 
amalgamateR :: [ ([Int],Word) ] -> [ ([Int],Word) ]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)-}    
  
  