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

{-blackPoint :: (Int, Int) -> Picture -> Picture
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
	|otherwise = selectRow (n+1) y rs -}

blackPoint :: (Int,Int) -> Picture -> Picture
blackPoint (x,0) (p:ps) = (replacePoint x p):ps
  where
  replacePoint :: Int -> String -> String
  replacePoint 0 (e:es) = "#" ++ es
  replacePoint x (e:es) = e:replacePoint (x-1) es
blackPoint (x,y) (p:ps) = p:blackPoint (x,y-1) ps
	

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
superimposeRep (h, w, points) (h', w', points') = (h, w, dropDouble (points++points'))
  where
  dropDouble [] = []
  dropDouble (x:xs)
   | x `notElem` xs = x : dropDouble xs
   | otherwise = dropDouble xs
	
rotate90Rep :: Rep -> Rep 
rotate90Rep (h,w, points) = flipVRep (w,h, map rotate90Point points)

rotate90Point :: (Int, Int) -> (Int, Int)
rotate90Point (x,y) = (y,x) 
	
--10.19
          
--Redo 6.2 exercises 

type Position = (Int, Int)
type Img = (Rep, Position)

imgPosition = snd
xCoord = fst
yCoord = snd

makeImg :: Rep -> Position -> Img
makeImg rep pos = (rep, pos)

horseImg :: Img
horseImg = makeImg (pictureToRep horse) (13,13) 

changePositionImg :: Img -> Position -> Img 
changePositionImg img pos = (fst img, pos)

makePosition :: Int -> Int -> Position 
makePosition a b = (a, b)

moveImg :: Img -> Int -> Int -> Img
moveImg img moveX moveY = changePositionImg img ((xCoord(imgPosition img)+ moveX), (yCoord(imgPosition img) + moveY))

place :: Img -> Img
place ((h, w, points), (x,y)) = ((h+y, w+x, map (shiftPointsX x) points), (0,0))

shiftPointsX :: Int -> (Int, Int) -> (Int, Int)
shiftPointsX w (x,y) = (x+w, y)

showPicture :: Rep -> Picture
showPicture (height, width, listofblackpts) = foldr blackPoint (whitePicture height width) listofblackpts 

printImg :: Img -> IO()
printImg = printPicture. showPicture. fst. place 

flipHImgN :: Img -> Img 
flipHImgN img = ((flipHRep.fst) img, snd img)

flipVImgN :: Img -> Img 
flipVImgN img = ((flipVRep.fst) img, snd img)

rotateImgN :: Img -> Img
rotateImgN img = ((rotateRep.fst) img, snd img)

rotate90ImgN :: Img -> Img 
rotate90ImgN img = ((rotate90Rep.fst) img, (rotate90Point.snd) img)

flipHImg :: Img -> Img 
flipHImg img@((h,w,point),(x,y)) = moveImg (flipHImgN img) 0 (-h) 

flipVImg :: Img -> Img 
flipVImg img@((h,w,point),(x,y)) = moveImg (flipVImgN img) (-w) 0 

rotateImg :: Img -> Img
rotateImg img@((h,w,point),(x,y)) = moveImg (rotateImgN img) (-w) (-h) 

rotate90Img :: Img -> Img
rotate90Img img@((h,w,point),(x,y)) = moveImg (rotate90ImgN img) (-h) (-w)

superimposeImg :: Img -> Img -> Img  
superimposeImg img1 img2 = ((superimposeRep ((fst.place) img1) ((fst.place) img2)), (0,0))

--10.20

type Doc  = String
type Line = String
type Word = String

docEx :: Doc
docEx = "catherdral doggerel catherdral\nbattery doggerel is catherdral\ncatherdral" 

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
    
{-amalgamate :: [ ([Int],Word) ] -> [ ([Int],Word) ]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2    = (l1,w1) : amalgamate ((l2,w2):rest)
  | otherwise   = amalgamate ((l1++l2,w1):rest)-}    
 
type Pages = String  
  
makeIndexRange :: Doc -> [(Pages, Word)]  
makeIndexRange = makeIndex >.> (map ranger)

ranger :: ([Int],Word) -> (Pages, Word) 
ranger l = ((((intercalate ",").makeRange.fst) l), snd l)   

intercalate :: String ->  [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate joiner (x:xs) = x ++ joiner ++ (intercalate joiner xs)  

{-makeRange :: [Int] -> [Pages] 
makeRange [] = []
makeRange [n] = [show n] 
makeRange (n:n':ns) 
	|n' == n+1 = (show n ++"-"++ show n') : makeRange ns 
	|otherwise = show n : makeRange (n':ns)-} 
	
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

makeRange :: [Int] -> [Pages]
makeRange [] = []
makeRange (x:xs) = range : makeRange xs'
  where
  (range,xs') = makeRange' x x xs
  
  makeRange' :: Int -> Int -> [Int] -> (String,[Int])
  makeRange' s prior [] = (smartRange s prior, [])
  makeRange' s prior yss@(y:ys)
    | prior == y-1 = makeRange' s y ys
    | otherwise = (smartRange s prior, yss)

  smartRange a b
    | a == b = show a
    | otherwise = (show a) ++ "-" ++ (show b)

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

amalgamateR :: [([Int],Word) ] -> [[([Int],Word)]]
amalgamateR [] = []
amalgamateR p@((l,w):rest) = ((getUntil ((/=w).snd)) p) : amalgamateR (dropUntil ((==w).snd) p) 

--([1],"cat"), ([2],"cat") = ([1,2], cat) 
amal :: [([Int],Word) ] -> ([Int],Word)
amal p= (foldr (++) [] (map fst p), snd (head p))

amalgamate :: [([Int],Word) ] -> [([Int],Word)]
amalgamate p = map amal (amalgamateR p) 

--10.25

--See defination of shorten 

--10.26

--If the word is in the index more than twice the the entries will not all be amalgamted together

--10.27

printIndex :: [([Int], Word)] -> IO ()
printIndex = putStrLn . showIndex 

showIndex :: [([Int], Word)] -> String
showIndex p = intercalate "\n" (map showLine p) 
	where
	showLine (l,w)= (w++" "++(intercalate "," (map show l)))
	
--10.28

allNumWordsSmall :: [ ( Int , Line ) ] -> [ ( Int , Word ) ]
allNumWordsSmall = (filter (\(nl, wd) -> (length wd > 3))). concat. map numWords

--10.29

makeIndexLen :: Doc -> [ (Int,Word) ]
makeIndexLen
  = lines       >.>     --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten     >.>     --   [([Int],Word)] -> [([Int],Word)]  
    numberOfEntrys      --   [([Int],Word)] -> [(Int,Word)]

numberOfEntrys :: [([Int],Word)] -> [(Int,Word)]
numberOfEntrys p = map toLen p 
	where
	toLen (l,w) = (length l, w)

--10.30

ord :: Char -> Int
ord  =  fromEnum

chr  :: Int  -> Char
chr  =  toEnum

offset :: Int
offset = ord 'a' - ord 'A'

toLower :: Char -> Char
toLower ch
	|(ord ch) >97 	=ch
	|otherwise	    =chr(ord ch + offset)

lowerWord w = (toLower (head w)) : tail w 

splitWordsLow :: String -> [Word]
splitWordsLow [] = [] 
splitWordsLow st = (lowerWord (getWord st)): splitWordsLow (dropSpace (dropWord st))

--have numWords use splitWordsLow instead of splitWords

--What do I do with proper nouns? I could make everything Uppercase?

--10.31 
    
sortLsProp :: (a -> a -> Bool) -> [a] -> [a]
sortLsProp _ [] = []
sortLsProp property (p:ps)     
	 = sortLsProp property smaller ++ [p] ++ sortLsProp property larger
    where
    smaller = [ q | q<-ps , property q p ]
    larger  = [ q | q<-ps , property p q ]
		
--10.32

--makeIndexHaskell :: Doc -> String
--What would I want to ignore? 
--whitespaceH :: String
--whitespaceH = " \n\t;:.,\'\"!?()->|&+-"
--What else would I want to modify? 
--Its unclear why I would want an index of a progam couldn't I just Ctrl + F 

--10.33

{-id x             =  x                 --(id.1)
f . g            =  \ x -> f (g x)      --(..1)
flip f x y       =  f y x               --(flip.1)

Prove by principle of extensionality
f . (g . h) = f . (g . h) 
                     
f . (g (h x))                           by(..1) 
f (g (h x))                             by(..1) 

f . (g (h x))                           by(..1) 
f (g (h x))                             by(..1)-}

--10.34

{-Prove by principle of extensionality
id . f = f 

id (f x)                                by(..1)
f x                                     by(id.1)

f x-}

--10.35

{-Prove by principle of extensionality
flip . flip = id

(flip . flip) f x y 
flip (flip f x y)                           by(..1)
flip (f y x)                                by(flip.1)
f x y                                       by(flip.1)

(id) f x y                   
f x y                                       by(id.1)-}

--10.36

{-curry f x y      =  f (x, y)               --(curry.1)

uncurry f p      =  f (fst p) (snd p)      --(uncurry.1)

Prove that curry and uncurrt are inverses 

curry . uncurry = id                       by extensionality (below)    

(curry. uncurry) f (x, y)                  
curry (uncurry f (x,y))                    by(..1)
curry (f x y)                              by(uncurry.1) 
f (x,y)                                    by(curry.1)

(id) f (x,y)     
f (x,y)                                    by(id.1)

uncurry . curry = id                       by extensionality (below)    

(uncurry. curry) f (x, y)                  
uncurry (curry f (x,y))                    by(..1)
uncurry (f x y)                            by(curry.1) 
f (x,y)                                    by(uncurry.1)

(id) f (x,y)     
f (x,y)                                    by(id.1)-}

--10.37

{-iter n f x 
	|n ==0 = x                            --(iter.1)
    |n ==1 = f x                          --(iter.2)
	|n > 1 = iter (n-1) f (f x)           --(iter.3)

Prove for all real numbers 
iter n id = id 

if n=0
iter 0 id x                              
x                                       by(iter.1)

if n=1
iter 1 id x
id x                                    by(iter.2)
x                                       by(id.1)

if n>1
iter n f x  
iter (n-1) id (id x)                   by(iter.2)
n is decreasing and so will eventually reach the base cases. 
--What are the exceptable assumptions/syntax for showing something for all real numbers? -}

--10.38

{-f . f = f                             --(idempotent)

--abs is given in the prelude but its not clear how to me?

assume x is positve number
--What are the exceptable assumptions/syntax for this?
(abs . abs) x 
abs (abs x)
abs x
x

(abs . abs) (negate x) 
abs (abs  (negate x))
abs x
x

abs x
x
signum x = x
abs (negate x)
x

abs.abs = abs

signum returns -1 for negative numbers, 0 for zero, and 1 for positive numbers

if x is negative 
signum.signum
signum (signum x) 
signum (-1)
-1

if x is zero 
signum.signum
signum (signum x) 
signum (0)
0

if x is positive
signum.signum
signum (signum x) 
signum (1)
1

if x is negative 
sigum x
-1

if x is zero 
signum x
0

if x is positive
signum x
1

signum . signum = signum -}

--10.39

--More Proofs!