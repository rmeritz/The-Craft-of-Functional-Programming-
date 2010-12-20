--The Craft of Functional Programming 
--Ch. 6 Programming with Lists


module Ch6ex where

import Prelude hiding (lookup)
import Ch5ex 

--6.1

superimposeChar:: Char -> Char -> Char
superimposeChar ch1 ch2 
  |ch1==ch2 &&  ch1=='.'        ='.'
  |otherwise                        ='#'
  
--6.2

superimposeLine:: [Char] -> [Char] -> [Char]
superimposeLine chars1 chars2 = [superimposeChar ch1 ch2 |(ch1, ch2) <- zip chars1 chars2 ] 

--6.3

type Picture = [[Char]]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine chars1 chars2 | (chars1, chars2) <- zip pic1 pic2 ]

--6.4

printPicture :: Picture -> IO()
printPicture pic = putStrLn (onSeperateLines pic)

--6.6

--To get one element of a line of the picture where x is the element number of the line
anElement :: [Char] -> Int -> Char
anElement line x = line!!x

oneLineRotate90 :: Picture -> Int -> [Char]
oneLineRotate90 pic elementofline = reverse [anElement line elementofline | line <- pic ]

rotate90 :: Picture -> Picture 
rotate90 pic = [oneLineRotate90 pic n | n <- [0..(length(pic!!0)-1)] ]

--6.7

antiRotate90 ::Picture -> Picture
antiRotate90 pic = reverse [reverse (oneLineRotate90 pic n) | n <- [0..(length(pic!!0)-1)] ]

--6.8

oneElementScale :: Char -> Int -> [Char]
oneElementScale element scalingnumber = replicate scalingnumber element

oneLineScaleH :: [Char] -> Int -> [Char]
oneLineScaleH line scalingnumber
        |line/=""        = (oneElementScale (head(line)) scalingnumber) ++ (oneLineScaleH (drop 1 line) scalingnumber)
        |otherwise         =""

oneLineScaleHV :: [Char] -> Int -> Picture
oneLineScaleHV line scalingnumber = --[oneLineScaleH line scalingnumber | n <- [1..scalingnumber]]
        replicate scalingnumber (oneLineScaleH line scalingnumber)

scale :: Picture -> Int -> Picture
scale pic scalingnumber
        | pic/=[]         =(oneLineScaleHV (head(pic)) scalingnumber) ++ (scale (drop 1 pic) scalingnumber) 
        | otherwise          =[]

--6.9

horse :: Picture

horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]


type Position = (Int, Int) 
type Image = (Picture, Position)

small:: Image
small=(["12","34","56"], (5,5))

horseimg ::Image
horseimg = (horse, (3,7))

horseimg2 :: Image
horseimg2 = (horse, (0,0))

makeImage :: Picture -> Position -> Image
makeImage pic pos = (pic, pos)

--6.10

frt (x,y) = x

scd(x,y)=y
 
changePosition :: Image -> Position -> Image 
changePosition img newpos = (frt img, newpos)
  
--6.11

makePosition :: Int -> Int -> Position 
makePosition a b = (a, b)

moveImage :: Image -> Int -> Int -> Image
moveImage img moveX moveY = changePosition img ((frt(scd img)+ moveX), (scd(scd img) + moveY))

--6.12

addWhiteX :: Picture -> Int -> Picture
addWhiteX pic x = [addSpace line x | line <- pic ]

makeWhiteY :: Int -> String
makeWhiteY 0 = ""
makeWhiteY y = (makeWhiteY (y - 1)) ++ "\n"

addWhiteYLine :: String -> Int ->  String
addWhiteYLine lastline y = lastline ++ (makeWhiteY y)

addWhiteY :: Picture -> Int -> Picture
addWhiteY pic y = (init pic) ++ [addWhiteYLine (last pic) y]

place :: Image -> Picture
place img = addWhiteY (addWhiteX (frt img)(frt(scd(img)))) (scd(scd(img)))

printImage ::Image -> IO()
printImage img = printPicture (place img)

--6.13

flipV :: Picture -> Picture
flipV = map reverse

flipH :: Picture -> Picture
flipH = reverse

rotate :: Picture -> Picture
rotate = flipH . flipV

naiveFlipH :: Image -> Image 
naiveFlipH img = ((flipH (frt img)), (scd img))

naiveFlipV :: Image -> Image 
naiveFlipV img = ((flipV (frt img)), (scd img))

naiveRotate :: Image -> Image 
naiveRotate img = ((rotate (frt img)), (scd img))

naiveRotate90 :: Image -> Image 
naiveRotate90 img = ((rotate90 (frt img)), (scd img))

--6.14

geometricalFlipH :: Image -> Image 
geometricalFlipH img = moveImage (naiveFlipH img) 0 (negate(length (frt img)+1)) 

geometricalFlipV :: Image -> Image 
geometricalFlipV img = moveImage (naiveFlipV img) (negate(length (head (frt img))+1)) 0 

geometricalRotate :: Image -> Image
geometricalRotate img = moveImage (naiveRotate img) (negate(length (head (frt img))+1)) (negate(length (frt img)+1))

geometricalRotate90 :: Image -> Image
geometricalRotate90 img = moveImage (naiveRotate90 img) (negate(length (head (frt img))+1)) (negate(length (frt img)+1))

--6.15

addSpaceRight :: String -> Int -> String
addSpaceRight string 0 = string 
addSpaceRight string n = addSpaceRight string (n-1) ++ "."

addSpaceLeft :: String -> Int -> String
addSpaceLeft string 0 = string 
addSpaceLeft string n =    "."++ addSpaceLeft string (n-1)

addWhiteXRight :: Picture -> Int -> Picture
addWhiteXRight pic x = [addSpaceRight line x | line <- pic ]

addWhiteXLeft :: Picture -> Int -> Picture
addWhiteXLeft pic x = [addSpaceLeft line x | line <- pic ]

whiteLine :: Int -> String
whiteLine 0 = []
whiteLine x = whiteLine (x-1) ++ "."

addWhiteAbove :: Picture -> Int -> Int-> Picture
addWhiteAbove pic 0 width     = pic
addWhiteAbove pic above width = [(whiteLine width)] ++ (addWhiteAbove pic (above-1) width) 

addWhiteBelow :: Picture -> Int -> Int-> Picture
addWhiteBelow pic 0 width     = pic
addWhiteBelow pic below width = (addWhiteBelow pic (below-1) width) ++ [(whiteLine width)]

padOut :: Picture -> Int-> Int-> Int-> Int-> Int-> Picture
padOut pic left right above below width =  addWhiteXRight (addWhiteXLeft (addWhiteAbove (addWhiteBelow pic below width) above width) left) right

--6.16

imageLength :: Image -> Int
imageLength img = (frt(scd img)) + (length (head(frt(img))) )

imageHeight:: Image -> Int
imageHeight img = (scd(scd img)) + (length (frt img))

width :: Image -> Image -> Int
width img1 img2 = max (imageLength img1) (imageLength img2)

left :: Image -> Int
left img= frt (scd img)

right :: Image -> Image -> Int
right img1 img2
  |((width img1 img2)- (imageLength img1)) >0 = ((width img1 img2)- (imageLength img1))
  | otherwise =0

height :: Image -> Image -> Int
height img1 img2 = max (imageHeight img1) (imageHeight img2)

above :: Image -> Image -> Int
above img1 img2 
  |((height img1 img2)- (imageHeight img1)) >0 = ((height img1 img2)- (imageHeight img1))
  |otherwise = 0

below :: Image -> Int  
below img = scd(scd img)
    
superimposeImage :: Image -> Image -> Image
superimposeImage img1 img2 = makeImage (superimpose (padOut (frt img1) (left img1) (right img1 img2) (above img1 img2) (below img1) (width img1 img2)) (padOut (frt img2) (left img2) (right img2 img1) (above img2 img1) (below img2) (width img2 img1))) (0,0)

--6.18

maxThreeOccursLocal :: Int -> Int -> Int -> (Int, Int)
maxThreeOccursLocal x y z = (maxThreeLocal x y z, occursLocal x y z) 
  where 
  maxThreeLocal :: Int -> Int -> Int -> Int
  maxThreeLocal x y z = max (max x y) z
  
  threeEqualLocal :: Int -> Int -> Int -> Bool
  threeEqualLocal m n p = (m==n) && (m==p)
    
  twoEqualLocal :: Int -> Int -> Bool 
  twoEqualLocal m n = (m == n)
    
  fourEqualLocal :: Int -> Int-> Int -> Int-> Bool
  fourEqualLocal m n p q = (m==n) && (m==p) && (m==q)
    
  howManyOfFourEqualLocal :: Int-> Int -> Int -> Int -> Int
  howManyOfFourEqualLocal a b c d
    	|fourEqualLocal a b c d			                       =4
    	|threeEqualLocal a b c || threeEqualLocal b c d || threeEqualLocal c d a || threeEqualLocal d a b      =3
    	|twoEqualLocal a b || twoEqualLocal b c || twoEqualLocal c d ||twoEqualLocal d a   =2
	|otherwise	=1
  
  occursLocal :: Int -> Int -> Int -> Int
  occursLocal x y z = (howManyOfFourEqualLocal (maxThreeLocal x y z) x y z) -1
 
--6.20

type Name = String
type Price = Int
type BarCode =Int
type DatabaseSL = [(BarCode, Name, Price)]

codeIndex :: DatabaseSL
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

type TillType = [BarCode]
type BillType = [(Name,Price)]

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

--6.21
  
formatLine :: (Name, Price) -> String
formatLine (name, price) = name ++ (whiteLine n) ++ (formatPence price) ++ "\n"
 where
 n=lineLength - (length name) - (length (formatPence price))

--6.22

example :: [(Name, Price)]
example =[("Fish Fingers", 1231), ("Nappies" , 1010),("Orange Jelly", 56)]

formatLinesHelper :: [(Name, Price)] -> Int -> String
formatLinesHelper list (-1) = ""
formatLinesHelper list n = (formatLinesHelper list (n-1)) ++ formatLine (list!!n)

formatLines :: [(Name, Price)] -> String
formatLines list = formatLinesHelper list ((length list)-1)

--6.23

makeTotalHelper :: [(Name, Price)] -> Int -> Int
makeTotalHelper list (-1) = 0
makeTotalHelper list n = (makeTotalHelper list (n-1)) + (scd(list!!n))

makeTotal :: BillType -> Price
makeTotal list = makeTotalHelper list ((length list)-1)

--6.24

formatTotal :: Price -> String
formatTotal total= "\nTotal" ++ (whiteLine n) ++ (formatPence total)
 where
 n=lineLength - 5 - (length (formatPence total))

--6.25

formatBill :: BillType -> String
formatBill list = "\tHaskell Store\n\n"++(formatLines list)++(formatTotal(makeTotal list))

--6.26

look:: DatabaseSL -> BarCode -> (Name, Price)
look dBase findCode 
  |n == [] =("Unknown Item", 0)
  |n /=[]   =head(n)
  where 
  n=[(name, price) | (code, name, price) <-dBase, code==findCode]

--6.27

lookup :: BarCode -> (Name, Price)
lookup code = look codeIndex code

--6.28

exampleTill :: TillType
exampleTill = [1111, 1110, 1112]

makeBill :: TillType -> BillType
makeBill codeList = [lookup n | n<-codeList ] 

--6.29

example2 :: [(Name, Price)]
example2 =[("Fish Fingers", 1231), ("Dry Sherry, 1lt", 540), ("Nappies" , 1010),("Orange Jelly", 56), ("Dry Sherry, 1lt", 540)]

makeDiscount:: BillType -> Int
makeDiscount bill= n*100
  where 
  n= length [("Dry Sherry, 1lt", 540) | ("Dry Sherry, 1lt", 540) <- bill]
  
formatDiscount :: Price -> String
formatDiscount discount= "\nDiscount" ++ (whiteLine n)++(formatPence discount)++"\n"
 where
 n=lineLength - 8 - (length (formatPence discount))

formatBillBetter :: BillType -> String
formatBillBetter list = "\tHaskell Store\n\n"++(formatLines list)++(formatDiscount(makeDiscount list))++(formatTotal((makeTotal list) - (makeDiscount list)))

--6.30

removeCode :: BarCode -> DatabaseSL -> DatabaseSL
removeCode code dBase = [(c,n,p) |(c,n,p) <- dBase, c/=code ] 

exnew1::(BarCode, Name, Price)
exnew1= (4719, "Balls" , 221)

exnew2::(BarCode, Name, Price)
exnew2= (4712, "Bigger Balls" , 2281)

newCode :: (BarCode, Name, Price) -> DatabaseSL
newCode (c,n,p) = [(c,n,p)]++(removeCode c codeIndex)

--6.31

lookmod:: DatabaseSL -> BarCode -> [(Name, Price)]
lookmod dBase findCode = [(name, price) | (code, name, price) <-dBase, code==findCode]

makeBillmodHelper:: TillType -> Int -> BillType
makeBillmodHelper codelist (-1) =[]
makeBillmodHelper codelist n= (makeBillmodHelper codelist (n-1)) ++ (lookmod codeIndex (codelist!!n))

makeBillmod :: TillType -> BillType
makeBillmod codeList = makeBillmodHelper codeList ((length codeList)-1)
