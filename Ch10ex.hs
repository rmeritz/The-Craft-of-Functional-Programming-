--The Craft of Functional Programming 
--Ch. 10 Functions as Values

import Prelude hiding (succ, flip, elem, not)
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
flipVRep (h, w, points) = (h, w, map (flipVPoint h) points)

flipVPoint :: Int ->  (Int,Int) -> (Int, Int)
flipVPoint h (x,y) = (x, h-y)

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
--filter ((head list)/=) list
	
--10.19

--Redo 6.2 exercises 

type Position = (Int, Int)
type Img = (Rep, Position)

makeImg :: Rep -> Position -> Img
makeImg rep pos = (rep, pos)

frt (x,_)=x
scd (_,y)=y

changePositionRep :: Img -> Position -> Img 
changePositionRep img pos = (frt img, pos)

makePosition :: Int -> Int -> Position 
makePosition a b = (a, b)

moveImageRep :: Img -> Int -> Int -> Img
moveImageRep img moveX moveY = changePositionRep img ((frt(scd img)+ moveX), (scd(scd img) + moveY))

place :: Img -> Img
place ((h, w, points), (x,y)) = ((h+y, w+x, map (shiftPointsX x) points), (0,0))

shiftPointsX :: Int -> (Int, Int) -> (Int, Int)
shiftPointsX w (x,y) = (x+w, y)

showPicture :: Rep -> Picture
showPicture (height, width, listofblackpts) = foldr blackPoint (whitePicture height width) listofblackpts 

printImg :: Img -> IO()
printImg = printPicture. showPicture. frt. place 

