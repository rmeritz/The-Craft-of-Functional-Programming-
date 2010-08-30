--The Craft of Functional Programming 
--Ch. 7 Defining Functions Over Lists

import Prelude hiding (product, and, or)

--7.1

frtintplusone :: [Int] -> Int 
frtintplusone [] = 0
frtintplusone (x:xs) = x + 1

--7.2

frsttwo:: [Int] -> Int
frsttwo [] = 0
frsttwo [x] = x
frsttwo(x:xs) = x + (x:xs)!!1

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

unique :: [Int] -> [Int] 
unique [] = []
unique (x:xs)
  |elemNum x (x:xs) ==1 && (elemNum x (unique (xs)) ==0) = x:unique xs
  |otherwise = unique xs