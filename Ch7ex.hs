--The Craft of Functional Programming 
--Ch. 7 Defining Functions Over Lists

import Prelude hiding (product, and, or, reverse, unzip, drop, splitAt, take, zip3)

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
--I don't really understand this one.

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

{- Why don't I work ? Types...
insPair :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insPair (x,y) [] = [(x,y)]
insPair (x,y) ((z,w):(zs,ws))
 |x < z = (x,y):[(z,w):(zs,ws)]
 |x == z && y < w  = (x,y):[(z,w):(zs,ws)]
 |otherwise = (z,w): (insPair (x,y) (zs,ws))

pairISort :: [(Int, Int)] -> [(Int, Int)]
pairISort []     = [] 
pairISort ((x,y):(xs,ys)) = insPair (x,y) (pairISort (xs,ys))
-}

--7.14

drop :: Int -> [a] -> [a]
drop _ [] = [] 
drop n (x:xs)
  |n>0 = drop (n-1) xs
  |n==0 =(x:xs)
  |n<0 = error "PreludeList.drop: negative argument"

{-More issues with reccursion on paired types
splitAt ::Int -> [a] -> ([a],[a])
splitAt _ [] = ([],[])
splitAt _ [x] = ([x],[])
splitAt n (x:xs)
  |n > 0 = (x,(splitAt (n-1) xs)) 
  |n<=0 = ([],(x:xs))
-}

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

{-Even more issues making zip3 using zip
nomatchzip3 ::[a] -> [b] -> [c] -> [(a,b,c)]
nomatchzip3 x y z = (((zip x y)!!1), (z!!1))
-}