--The Craft of Functional Programing 
--Ch. 5 Data Types: Tuples and Lists

module Ch5ex where

import Ch4ex 
import Ch3exercises

--5.1

maxOccurs :: Int -> Int -> (Int, Int)  
maxOccurs x y 
	|(x == max x y) && (y == max x y) = (max x y, 2)
	| otherwise 			  = (max x y, 1)


maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z = (maxThree x y z, (howManyOfFourEqual (maxThree x y z) x y z) -1 ) 
			
--5.2

middleNumber x y z
	|between y x z	= x
	|between x y z  = y
	|otherwise 	= z

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z) = (maxThree x y z, middleNumber x y z, minThree x y z) 

--5.3

xInt:: (Float, Float) -> Float
xInt (slope, yint) = negate (slope/yint)

--5.8

doubleAll:: [Int] -> [Int]
doubleAll ex = [2*n| n <- ex ]

--5.9

capitalizeLetters:: String -> String 
capitalizeLetters ex = [ toUpper ch | ch <- ex ]

--5.10

divisor :: Int -> Int -> Bool
divisor x i 
	|mod x i ==0		=True
	|otherwise 		=False


divisors :: Int -> [Int]
divisors n = [x | x <- [n, (n-1) .. 1], divisor n x ] 

isPrime :: Int -> Bool
isPrime n
	|divisors n == [n, 1]   =True
	|otherwise 		=False

--5.11

matches :: Int -> [Int] -> [Int] 
matches n list = [ x | x <- list , x==n] 

element :: Int -> [Int] -> Bool
element n list 
	|matches n list == [] = False
	|otherwise         = True
	
--5.12

type Person =String
type Book   =String

type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice", "Tintin"), ("Anna", "Little Women"), ("Alice", "Asterix"), ("Rory", "Tintin")]

books:: Database -> Person -> [Book]
books dBase findPerson = [book | (person, book) <- dBase, person==findPerson]

--5.13

borrowers ::Database -> Book -> [Person]
borrowers dBase findBook = [person | (person, book) <-dBase, book==findBook]

borrowed :: Database -> Book -> Bool 
borrowed dBase book
	|borrowers dBase book ==[] =False
	|otherwise		   =True
	
numBorrowed :: Database -> Book -> Int
numBorrowed dBase book = length (borrowers dBase book)

--5.14

returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase pers bk = [pair | pair <-dBase, pair /=(pers, bk)]

--5.16

sing x = [x]	

--5.18

shift((x, y), z) = (x, (y, z))

--5.20

romanDigit :: Char -> String
romanDigit n 
	|n=='1' 	="I"
	|ord n==50	="II"
	|ord n==51      ="III"
	|ord n==52	="IV"
	|ord n==53	="V"
	|ord n==54	="VI"
	|ord n==55	="VII"
	|ord n==56	="VIII"
	|ord n==57	="IX"
	
--5.21

onThreeLines :: String -> String -> String -> String 
onThreeLines a b c = a++"\n"++b++"\n"++c

--5.22

each :: [String] -> Int -> String 
each list n
	|n>=0  	   = each list (n-1) ++ list!!n ++ "\n"
	|otherwise =[]

onSeperateLines :: [String] -> String
onSeperateLines list = each list ((length list)-2) ++ list!!((length list)-1)

--["Join", "me", "please"] "Join\nme\nplease\n"

--5.23

duplicate :: String -> Int -> String 
duplicate string n 
	|n>1 		=string ++ duplicate string (n-1)
	|n==0 		=""


--5.24

addSpace :: String -> Int -> String
addSpace string 0 = string 
addSpace string n =    " "++addSpace string (n-1)

linelength=8

pushRight :: String -> String 
pushRight string =addSpace string (linelength-length(string))

--5.25

pushRightOrLeft :: String -> String 
pushRightOrLeft string 
	|linelength>= length(string) = addSpace string (linelength-length(string))
	|linelength<length(string)   = take linelength string 

--5.26

fibStep :: (Int,Int) -> (Int, Int)
fibStep (u, v) =(v, u+v)

fibPair :: Int -> (Int, Int) 
fibPair 0 = (0,1)
fibPair n = fibStep(fibPair (n-1))

fastFib :: Int -> Int
fastFib = fst.fibPair


fibTable::Int -> String 
fibTable (-1) = "\tn\tfib n"
fibTable n	      =  fibTable(n-1)++"\n\t"++show(n)++"\t"++show(fastFib n)

