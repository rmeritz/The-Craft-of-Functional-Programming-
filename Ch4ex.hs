--The Craft of Functional Programing 
--Ch. 4 Designing and Writiing Programs

module Ch4ex where

--4.1

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max (max x y) z

maxFour:: Int -> Int -> Int -> Int -> Int 
--maxFour w x y z = max ( max w x) (max y z)
--maxFour w x y z = max (maxThree w x y) z
maxFour w x y z = maxThree (maxThree w x y) (maxThree x y z) (maxThree y z w)

--4.2

weakAscendingOrder :: Int -> Int -> Int -> Bool 
weakAscendingOrder a b c
	|(a >= b) && (b >= c)                =True
	|otherwise                           =False
	
weakDesendingOrder :: Int -> Int -> Int -> Bool 
weakDesendingOrder a b c
	|(a <= b) && (b <= c)                =True
	|otherwise                           =False	

between :: Int -> Int -> Int -> Bool 
between a b c = weakAscendingOrder a b c || weakDesendingOrder a b c
	
--4.3 

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m==n) && (m==p)

twoEqual :: Int -> Int -> Bool 
twoEqual m n = (m == n)

howManyEqual :: Int-> Int -> Int -> Int
howManyEqual a b c 
	|threeEqual a b c == True 		=3
	|twoEqual a b || twoEqual b c		=2
	|otherwise				=0

--4.4

fourEqual :: Int -> Int-> Int -> Int-> Bool
fourEqual m n p q = (m==n) && (m==p) && (m==q)

howManyOfFourEqual :: Int-> Int -> Int -> Int -> Int
howManyOfFourEqual a b c d
	|fourEqual a b c d			                       =4
	|threeEqual a b c || threeEqual b c d || threeEqual c d a || threeEqual d a b      =3
	|twoEqual a b || twoEqual b c || twoEqual c d ||twoEqual d a   =2
	|otherwise				                       =0

--4.5

rangeProduct :: Int -> Int -> Int 
rangeProduct m n 
	| m > n  	= 0
	| n == m	= m
	| m < n  	= m * (rangeProduct (m+1) n)
	
--4.6	
	
fac :: Int -> Int
fac x
	| x > 0 	= rangeProduct 1 x 
	|otherwise	= error "fac only defined on natural numbers"
	
--4.7

multiply :: Int -> Int -> Int 
multiply m n  
	| n ==0 || m ==0 	=0
	| n>0                   =m + multiply m (n-1)

--4.8
 
square :: Int -> Int
square n = n^2

sqrtTest :: Int -> Int -> Int 
sqrtTest x a 
        |square a > x   = sqrtTest x (a-1)
	|square a <= x  = a    
   
sqrtInt :: Int -> Int 
--sqrtInt a =  floor (sqrt(fromIntegral a)) 
sqrtInt x = sqrtTest x (div x 2) 

--4.9

--Test Data
fun :: Int -> Int 
fun 0 = 7
fun 1 = 8
fun 2 = 0
fun 3 = 17
fun 4 = 10

maxF ::(Int -> Int) -> Int -> Int
maxF function n
	|n>0   = max (maxF function (n-1))(function n)
	
--4.10

zeroFun :: (Int -> Int) -> Int -> Bool 
zeroFun function n 
	| n>=0 && (function n) == 0                              = True
	| n>=0 && zeroFun function (n-1) == True                 = True
	| otherwise                                              = False
	
--4.11

sumFun :: (Int -> Int) -> Int -> Int
sumFun function n 
	| n== 0      = function 0
	| n>0	     = sumFun function (n-1) + function n 

regions :: Int -> Int
regions n
	|n==0 	=1
	|n>0    =regions (n-1) + n
	
regionsS :: Int -> Int 
regionsS n =sumFun id n +1
	
--4.13

factor :: Int -> Int -> Bool  
factor a b 
	|b>0 && mod a b ==0 	= True
	|otherwise   	        = False

highestFactor :: Int -> Int -> Int 
highestFactor x y
	| factor (max x y) (min x y) == True                  = min x y 
	| y>0 && factor x y ==False                           = highestFactor (max x y) ((min x y) -1)   
	| otherwise 	                                      = 0

highestCommonFactor :: Int -> Int -> Int
highestCommonFactor x y = highestFactor(highestFactor x y)(min x y)

--4.14


twoToN :: Int -> Int 
twoToN n 
	|n==1         = 2
	|even n       = twoToN (div n 2) * twoToN (div n 2) 
	|odd n        = twoToN (div n 2) * twoToN (div n 2) * 2

--4.16

solution m n p = ((m+n+p)==3*p)

--4.18

attempt m n p = (m/=n) && (n/=p)



