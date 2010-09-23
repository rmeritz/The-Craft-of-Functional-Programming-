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

--14.8

--My pattern matching doesn't work in trying to define == over Shapes in a new way 

{-instance Eq Shapes where
  ((Circle a) == (Circle b)) = True && ((a==b) || ((a<0) && (b<0))) = True
  ((Rectangle h w) == (Rectangle h' w')) && (((h==h') && (w==w')) || ((h==w') && (w==h'))) = True 
  ((Triangle a b c) == (Triangle d e f)) && (([a b c] == [d e f]) || ([a b c] == [e d f]) || ([a b c] == [f e d])) = True
  otherwise = False -}

--14.9

data NewShape  = CircleN Float Float Float | RectangleN Float Float Float Float | TriangleN Float Float Float Float Float
	deriving (Eq, Ord, Show, Read)

--14.10

move :: Float -> Float -> NewShape -> NewShape
move x' y' (CircleN r x y) = CircleN r x' y' 
move x' y' (RectangleN h w x y) = RectangleN h w x' y' 
move x' y' (TriangleN a b c x y) = TriangleN a b c x' y' 

--14.11

--New some more mathmatical thought to figure out how to tackle this one. 
overlap :: NewShape -> NewShape -> Bool 
overlap (CircleN r1 x1 y2) (CircleN r2 x2 y2) =