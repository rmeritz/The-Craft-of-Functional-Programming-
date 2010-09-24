--The Craft of Functional Programing 
--Ch. 14 Introducing Algebraic Types 

--14.1

data Temp   = Cold | Hot 
	deriving (Eq,Ord,Enum,Show,Read)
data Season = Spring | Summer | Autumn | Winter 
	deriving (Eq,Ord,Enum,Show,Read)

weather :: Season -> Temp
--weather Summer = Hot
--weather _      = Cold

weather s 
	|s==Summer = Hot
	|otherwise = Cold

--14.2

data Month = January | February | March | April | May | June | July |  August | September | October | November | December
	deriving (Eq,Ord,Enum,Show,Read)

monthToSeason :: Month -> Season
monthToSeason m 
	|filter (==m) [January .. March] /= [] = Winter
	|filter (==m) [April .. June] /= [] = Spring
	|filter (==m) [July .. September] /= [] = Summer
	|otherwise = Autumn

--14.3

weatherNZ :: Season -> Temp
weatherNZ Summer = Cold
weatherNZ _ = Hot

weatherBrazil :: Season -> Temp
weatherBrazil Summer = Cold
weatherBrazil Autumn = Cold
weatherBrazil _ = Hot

--14.4

data Shape  = Circle Float | Rectangle Float Float | Triangle Float Float Float
	deriving (Eq, Ord,Show,Read)

perimeter :: Shape -> Float 
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle h w) = 2*h*w
perimeter (Triangle a b c)= a+b+c

--14.5

isRound :: Shape -> Bool
isRound (Circle _)      = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r)      = pi*r*r
area (Rectangle h w) = h*w
area (Triangle a b c) = 0.25*sqrt((a+b-c)*(a-b+c)*(-a+b+c)*(a+b+c))

--14.6

isRegular :: Shape -> Bool 
isRegular (Circle _) = True
isRegular (Rectangle h w)
	|h==w = True
	|otherwise = False
isRegular (Triangle a b c)
	|a==b && b==c && c==a = True
	|otherwise = False
	
--Why went I use the function do my arguements need to be in parens?

--14.7

--The dervived ordering is left to right lowest to greatest. 
--The floats of the same shape are order lexiographially. 
--Exact documentation was found here http://www.haskell.org/onlinereport/derived.html

--14.8

--My pattern matching doesn't work in trying to define == over Shapes in a new way 

{-instance Eq Shapes where
  ((Circle a) == (Circle b)) = True && ((a==b) || ((a<0) && (b<0))) = True
  ((Rectangle h w) == (Rectangle h' w')) && (((h==h') && (w==w')) || ((h==w') && (w==h'))) = True 
  ((Triangle a b c) == (Triangle d e f)) && (([a b c] == [d e f]) || ([a b c] == [e d f]) || ([a b c] == [f e d])) = True
  otherwise = False -}

--14.9

type CenterPoint = (Float, Float)

type Radius = Float 

type Height = Float 

type Width = Float 

data NewShape  = CircleN Radius CenterPoint | RectangleN Height Width CenterPoint 
	deriving (Eq, Ord, Show, Read)

--14.10

exC1 :: NewShape
exC1 = CircleN 5 (0,0) 
exC2 :: NewShape 
exC2 = CircleN 3 (1,1) 
exC3 :: NewShape 
exC3 = CircleN 1 (3,5)
exR1 :: NewShape 
exR1 = RectangleN 5 5 (0,0)
exR2 :: NewShape 
exR2 = RectangleN 2 3 (0,4)
exR3 :: NewShape 
exR3 = RectangleN 1 7 (4,5)

move :: CenterPoint -> NewShape -> NewShape
move (x', y') (CircleN r (x, y))= CircleN r (x', y') 
move (x', y') (RectangleN h w (x, y))= RectangleN h w (x', y') 

--14.11

centerToCenterDistance :: CenterPoint -> CenterPoint -> Float
centerToCenterDistance (x1, y1) (x2, y2) = sqrt (((x1-x2)^2)+((y1-y2)^2)) 

xCrd = fst
yCrd = snd

xInRange :: Width -> CenterPoint -> Width -> CenterPoint -> Bool 
xInRange w1 c1 w2 c2 = (lowBound2 < lowBound1 && lowBound1 < highBound2)|| (lowBound2 < highBound1 && highBound1 < highBound2)
	where 
	lowBound1 = w1 - xCrd(c1)
	highBound1 = w1 + xCrd(c1)
	lowBound2 = w2 - xCrd(c2)
	highBound2 = w2 + xCrd(c2) 
	
yInRange :: Height -> CenterPoint -> Height -> CenterPoint -> Bool 
yInRange h1 c1 h2 c2 = (lowBound2 < lowBound1 && lowBound1 < highBound2)|| (lowBound2 < highBound1 && highBound1 < highBound2)
	where 
	lowBound1 = h1 - yCrd(c1)
	highBound1 = h1 + yCrd(c1)
	lowBound2 = h2 - yCrd(c2)
	highBound2 = h2 + yCrd(c2) 
	
overlap :: NewShape -> NewShape -> Bool 
overlap (CircleN r1 c1) (CircleN r2 c2) 
	|(centerToCenterDistance c1 c2) - r1 -r2 <=0 = True
	|otherwise = False 
overlap (RectangleN h1 w1 c1) (RectangleN h2 w2 c2)
	|(xInRange w1 c1 w2 c2) && (yInRange h1 c1 h2 c2) = True
	|otherwise = False 
--overlap (CircleN r c1) (RectangleN h w c2) 
--	|(centerToCenterDistance c1 c2) - r - w <=0 = True
--	|otherwise = False 

--I haven't figured out a clever way to do this with a rectangle and a circle. 
--I also just got rid of triangles. 

--14.12

data House = String | Int 
	deriving (Eq, Ord, Show, Read)

type City = String

type State = String 

type Zipcode = Int 

type Address = (House, City, State, Zipcode) 

--I don't know if I actually did what the problem statement asks for. 

--14.13

type Name = String 
type Book = String

data LibraryEntry = Entry Name Book 
	deriving (Eq, Ord, Show, Read)  

type LibraryLoans = [LibraryEntry]

exBib :: LibraryLoans
exBib = [Entry "Alice" "Tintin", Entry "Anna" "Little Women", Entry "Alice" "Asterix", Entry "Rory" "Tintin"]

entryToBook :: LibraryEntry -> Book 
entryToBook (Entry _ book) = book  

entryToPerson :: LibraryEntry -> Book 
entryToPerson (Entry name _ ) = name  

booksDoesHeHaveOnLoan :: LibraryLoans -> Name -> [Book]
booksDoesHeHaveOnLoan loans name = (map entryToBook . filter ((==name).entryToPerson)) loans  

whoHasTheBook :: LibraryLoans -> Book -> [Name]
whoHasTheBook loans book = (map entryToPerson . filter ((==book).entryToBook)) loans 

isBorrowed :: LibraryLoans -> Book -> Bool 
isBorrowed loans book = whoHasTheBook loans book /= []

numBorrowed :: LibraryLoans -> Book -> Int 
numBorrowed loans book = length (whoHasTheBook loans book) 

returnLoan :: LibraryLoans -> LibraryEntry -> LibraryLoans
returnLoan loans entry = filter (/=entry) loans

--14.14

type CD = String
type Video = String 

type Day = Int 
type Year = Int 

data Date = When Year Month Day
	deriving (Eq, Ord, Show, Read) 

type DueDate = Date

--No clear what the best way to handle dates is. 
--I think the way I have it now cheaks the order correctly but it also allows you to easily use invalid dates. 
--I don't think that the way I have it now makes it easy to update today's date. 
--People have made libraries that do some of this for me. 
--Can we talk about how to use them. See example below. 
--http://www.cse.chalmers.se/alumni/bringert/darcs/parsedate/doc/

data NewLibEntry = BookLoan Name Book DueDate | CDLoan Name CD DueDate | VidLoan Name Video DueDate  
	deriving (Eq, Ord, Show, Read)  

type NewLibLoans = [NewLibEntry]

exNewBib :: NewLibLoans
exNewBib = [BookLoan "Alice" "Tintin" (When 1990 May 3), BookLoan "Anna" "Little Women" (When 1999 June 30), CDLoan "Alice" "Asterix" (When 1999 June 1), VidLoan "Rory" "Tintin" (When 1999 May 3)]

type Author = String
type Artist = String

data BookInfo = Writer Author [Book] 
	deriving (Eq, Ord)

type BookList = [BookInfo]

data CDInfo = MusicMaker Artist [CD]

type CDList = [CDInfo]

entryToItem :: NewLibEntry -> String  
--entryToItem (_ _ item _) = item Why cannot I just do this?
entryToItem (BookLoan _ item _ ) = item
entryToItem (CDLoan _ item _ ) = item
entryToItem (VidLoan _ item _ ) = item 

entryToName :: NewLibEntry -> Name
entryToName (BookLoan name _ _ ) = name
entryToName (CDLoan name _ _ ) = name 
entryToName (VidLoan name _ _ ) = name 

entryToEntry :: NewLibEntry -> String 
entryToEntry (BookLoan _ _ _ ) = "BookLoan"
entryToEntry (CDLoan _ _ _ ) = "CDLoan"
entryToEntry (VidLoan _ _ _ ) = "VidLoan"

entryToDueDate :: NewLibEntry -> Date 
--Why not entryToDueDate :: NewLibEntry -> DueDate ?
entryToDueDate (BookLoan _ _ dd ) = dd
entryToDueDate (CDLoan _ _ dd ) = dd 
entryToDueDate (VidLoan _ _ dd ) = dd 

allBooksHeHasOnLoan :: NewLibLoans -> Name -> [Book]
allBooksHeHasOnLoan loan name = (map entryToItem . filter ((==name).entryToName). filter ((=="BookLoan").entryToEntry)) loan  

allCDsHeHasOnLoan :: NewLibLoans -> Name -> [CD]
allCDsHeHasOnLoan loan name = (map entryToItem . filter ((==name).entryToName). filter ((=="CDLoan").entryToEntry)) loan  

allVidsHeHasOnLoan :: NewLibLoans -> Name -> [Video]
allVidsHeHasOnLoan loan name = (map entryToItem . filter ((==name).entryToName). filter ((=="VidLoan").entryToEntry)) loan  

allHeHasOnLoanOneList :: NewLibLoans -> Name -> [String]
allHeHasOnLoanOneList loan name = (map entryToItem . filter ((==name).entryToName)) loan  

allHeHasOnLoan :: NewLibLoans -> Name -> ([Book],[CD],[Video]) 
allHeHasOnLoan loans name = (allBooksHeHasOnLoan loans name, allCDsHeHasOnLoan loans name, allVidsHeHasOnLoan loans name)

allDueBackBeforeThen :: NewLibLoans -> DueDate -> [String]
allDueBackBeforeThen loans date = (map entryToItem . filter((<date).entryToDueDate)) loans

personHasDueBeforeThen :: NewLibLoans -> Name -> DueDate -> [String]
personHasDueBeforeThen loans name date = (map entryToItem . filter((<date).entryToDueDate). filter ((==name).entryToName)) loans

monthCycle :: [Month]
monthCycle = [January .. December] ++ [January]

dropUntilMod :: (a -> Bool) -> [a] -> [a]
dropUntilMod p [] = []
dropUntilMod p (x:xs)
	|p x = dropUntilMod p xs
	|otherwise = xs

nextMonth :: Month -> Month
nextMonth m = head (dropUntilMod (/=m) monthCycle)
--Is there a cleaner way to write this?

makeDueDate :: NewLibEntry -> NewLibEntry
makeDueDate (BookLoan n bk dd ) = (BookLoan n bk (addMonth dd))
	where 
	addMonth (When year month day)
		|(month == December) = (When (year+1) January day) 
		|otherwise = (When year (nextMonth month) day) 

makeDueDate (CDLoan n cd dd ) = (CDLoan n cd (addWeek dd))
	where 
	addWeek (When year month day)
		|((day >= 23) && (month/= December)) = (When year (nextMonth month) (day+7-30))
		|(day >= 23) = (When (year+1) (nextMonth month) (day+7-30))
		|otherwise = (When year month (day+7))
		
makeDueDate (VidLoan n vid dd ) = (VidLoan n vid (add3Days dd))
	where
	add3Days (When year month day)
		|((day >= 27) && (month /= December)) = (When year (nextMonth month) (day+3-30))
		|(day >= 23) = (When (year+1) (nextMonth month) (day+3-30))
		|otherwise = (When year month (day+3))

--How do I deal with the fact that months are not all the same lenght?
--I probably what handle make an new data type with the days are a propert as the lenght of months somehow...
--Right now I'm just saying that they all have 30 days. 
--This doesn't work obviously. 

today :: Date 
today = When 2010 September 24

type LoanType = String 
type Item = String

addLoan :: NewLibLoans -> Name -> LoanType -> Item -> NewLibLoans 
addLoan existingLoans name typeOfLoan title 
	|(typeOfLoan == "Book") = (makeDueDate(BookLoan name title today)):existingLoans 
	|(typeOfLoan == "CD") = (makeDueDate(CDLoan name title today)):existingLoans 
	|(typeOfLoan == "Video") = (makeDueDate(VidLoan name title today)):existingLoans 
