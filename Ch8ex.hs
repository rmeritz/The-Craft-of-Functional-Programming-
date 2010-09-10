--The Craft of Functional Programming 
--Ch. 8 Reasoning about Programs

--8.1

fact :: Int -> Int
fact n
  | n==0        = 1
  | otherwise   = n * fact (n-1)

--(4>2) || (fact(-1)==17) -- True 
--(4>2) && (fact(-1)==17) --False

--8.2

mult :: Int -> Int -> Int
mult 0 _ = 0 
mult 1 n = n 
mult x n = n + (mult (x-1) n) 

--0 `mult` fact(-2) = 0 
--fact(-2) `mult` 0 = undefined
  

--8.3
{- Prove 
sum (xs ++ ys) = sum xs + sum ys   (hyp)  (sum++)


[] ++ zs = zs                      (++.1)
(w:ws) ++ zs = w:(ws++zs)          (++.2)
 
sum [] = 0                         (sum.1)
sum (x:xs) = x + sum xs            (sum.2)

(base)
sum([]++ys)= sum(ys)              by(++.1) 

sum [] + sum ys = 0 + sum ys      by(sum.1)
=sum ys 

(induction)
sum((x:xs)++ys)
= sum (x:(xs++ys))                by(++.2)
=x + sum(xs++ys)                  by(sum.2)
=x + sum xs + sum ys              by(hyp)

sum (x:xs) + sum ys 
= x + sum xs + sum                by(sum.2)
-}

--8.4
{- Prove 
xs ++ [] = xs                      (hyp) (++.3)

(base)
xs ++ [] = [] ++ []= []            by(++.1)

xs = [] 

(induction)
(x:xs) ++ []
=x:(xs++[])                        by(++.2)
=x:xs                              by(hyp)
 
(x:xs)
-}

{-Prove
xs ++ (ys ++ zs) = (xs ++ys) ++ zs (hyp)  (++.4)

(base)
[] ++ (ys ++ zs)
=(ys ++zs)                          by(++.1)

([]++ys) ++ zs
=ys ++ zs                           by(++.1)

(induction)
(x:xs) ++ (ys ++zs)
=x:(xs++(ys++zs))                   by(++.2)
=x:((xs ++ys) ++ zs)                by(hyp)

((x:xs)++ys)++zs
=(x:(xs++ys))++zs                   by(++.2)

--They look really close by the parens don't quite match
--do I need to show for finate ys and zs to complete the proof or is that just assigned as an exercise?
-}

--8.5

{-Prove
sum (reverse xs) = sum xs          (hyp)

reverse [] = []                    (reverse.1)
reverse (z:zs) = reverse zs ++ z   (reverse.2)

(base)
sum(reverse []) = sum ([])         by(reverse.1)
=0                                 by (sum.1)

sum [] = 0                         by (sum.1)

(induction)
sum(reverse (x:xs))
= sum(reverse xs ++ [x])           by(reverse.2)
= sum(reverse xs) + sum [x]        by(sum++)
= sum(reverse xs) + x              by(sum.2)
= sum xs + x                       by(hyp)

sum(x:xs)
= x + sum xs                       by(sum.2)
= sum xs + x                       by associative property of addtion 

-}

{-Prove 
length (reverse xs) = length xs

length [] = 0                            (length.1)
length (z:zs) = 1 + length zs            (length.2)
length (xs++ys) = lenght xs + length ys  (length.4)

(base) 
length (reverse []) = length ([]) by(reverse.1)
=0                                by(length.1)

length [] = 0                     by(lenght.1)

(induction)
length (reverse(x:xs))
=length(reverse xs ++ x)          by(reverse.2)
=length(reverse xs) + length x    by(length.4)
=length(reverse xs) + 1           by(length.2)
=length(xs) + 1                   by(hyp)

length(x:xs)
=1 + length xs                    by(length.2)
=length(xs) + 1                   by associative property of addtion

-}

--8.6

{- Prove 
elem z (xs ++ zs) = elem z xs || elem z zs   (hyp)

elem x [] = False                            (elem.1)
elem x (y:ys) = (x==y) || (elem x ys)        (elem.2)

(base)
elem z ([] ++ zs)
elem z zs                                     by(++.1)

elem z [] || elem z zs
False || elem z zs                            by(elem.1)
elem z zs                                     by ||

(induction)
elem z ((x:xs) ++ zs)
elem z (x:(xs ++ zs))                         by(++.2)
(z==x)||elem z (xs++zs)                       by(elem.2)
(z==x)||elem z xs || elem z zs                by(hyp)

elem z (x:xs) || elem z zs
(z==x)||elem z xs || elem z zs                by(elem.2)

-}

--8.7
{-Prove
zip (frt (unzip ps)) (scd(unzip ps)) = ps

unzip [] = ([],[])                            (unzip.1)
unzip ((x,y):ps) = (x:xs,y:ys)                (unzip.2)
  where (xs,ys)=unzip ps  
  
frt(x,y) = x                                  (frt.1)

scd(x,y) = y                                  (scd.1)

zip (x:xs) (y:ys) = (x,y) : zip xs ys         (zip.1)
zip (x:xs) []     = []                        (zip.2)
zip []     zs     = []                        (zip.3)

(base)
zip (frt (unzip [])) (scd(unzip []))         
=zip (frt ([],[])) (scd([],[]))               by(unzip.1)
=zip [] (scd([],[]))                          by(frt.1)
=zip [] []                                    by(scd.1)
=[]                                           by(zip.2)

[]

(induction)

where 
ps = (xs,ys)                       --can I just assume this form?
p:ps = ((x,y):(xs,ys))

zip (frt (unzip ((p:ps))) (scd(unzip ((p:ps)))
zip (frt (unzip ((x,y):(xs,ys))) (scd(unzip ((x,y):(xs,ys)))
zip (frt (x:xs,y:ys)) (scd(x:xs,y:ys))               by(unzip.2)
zip (x:xs) (scd(x:xs,y:ys))                          by(frt)
zip (x:xs) (y:ys)                                    by(scd)
(x,y) : zip xs ys                                    by(zip.1)

(p:ps)
((x,y):(xs,ys))                                      
zip (frt (unzip ((x,y):(xs,ys))) (scd(unzip ((x,y):(xs,ys))) by(hyp)
zip (frt (x:xs,y:ys)) (scd(x:xs,y:ys))               by(unzip.2)
zip (x:xs) (scd(x:xs,y:ys))                          by(frt)
zip (x:xs) (y:ys)                                    by(scd)
(x,y) : zip xs ys                                    by(zip.1)

--I don't feel this is quite right. Its seems like I jumped a step somewhere 
--Maybe its just different than the others

-}

--8.8 

{-Prove
take n xs ++ drop n xs = xs                         (hyp)

take 0 _        = []                                (take.1)
take _ []       = []                                (take.2)
take n (x:xs)                                       (take.3)
  | n>0         = x : take (n-1) xs                 
  
drop _ [] = []                                      (drop.1)
drop n (x:xs)                                       (drop.2)
  |n>0 = drop (n-1) xs
  |n==0 =(x:xs)

(base)
take n [] ++ drop n []
=[] ++ drop n []                                     by(take.2)
=[] ++ []                                            by(drop.1)
=[]                                                  by(++.1)

[]

(induction)
take n (x:xs) ++ drop n (x:xs)
(x: take (n-1) xs) ++ (drop n (x:xs))                by(take.2)
(x: take (n-1) xs) ++ (drop (n-1) xs)                by(drop.2)
x: ((take (n-1) xs) ++ (drop (n-1) xs))              by(++.2)
x:xs                                                 by(hyp)   
-- Can I do this or must I prove somehow that if n is defined (n+1) is defined)

x:xs
-}

--8.9

{-Prove
rev (xs ++ ys) = rev xs ++ rev ys
	
shunt []     ys = ys				       (shunt.1)
shunt (x:xs) ys = shunt xs (x:ys) 		   (shunt.2)

rev xs = shunt xs []				       (rev.1)

(base)
rev ([] ++ ys)                             
rev (ys)                                   (++.1)
shunt ys []                                (rev.1)

rev [] ++ rev ys                  
shunt [] [] ++ shunt ys []                 (rev.1)
[] ++ shunt ys []                          (shunt.1)
shunt ys []                                (++.1)

(induction)
rev ((x:xs) ++ ys)                         
rev (x:(xs++ys))                           (++.2)                 
shunt (x:(xs++ys)) []                      (rev.1)
shunt (xs++ys) [x]                         (shunt.2)

rev (x:xs) ++ rev ys 
shunt (x:xs) [] ++ shunt ys []             (rev.1)
shunt xs [x] ++ shunt ys []                (shunt.2)

Induction doesn't work must generalize proof goal.
How do I do this?
}

--8.10

{-Prove
fac n = fac2 n                             (hyp)

fac2 n = facAux n 1                        (fac2.1)

facAux 0 p = p                             (facAux.1)
facAux n p = facAux (n-1) (n*p)            (facAus.2)

Same problem as 8.9
}




