--module Intervals2 where
import List

type UpperBound = Int
type LowerBound = Int 
type Interval = (UpperBound, LowerBound)
type Variable = String

data Entry = E (Interval, Variable)
	deriving (Show, Read)

data Map = Map [Entry] 
	deriving (Show, Read)

exMap = Map [E((0,0),"a"),E((4,8),"c"), E((10,12),"e")]

newMap :: Map 
newMap = Map []
--(-1,0)
--(0,0) (0,1), (1,1) (1,2)  

addMap :: Entry -> Map -> Map
addMap e (Map []) = Map [e]
addMap (E e@(i,v)) (Map m@((E(i1,v1)):ms)) 
	|snd i < fst i1  = Map ((E e):m)
	|snd i isElem  
	|otherwise = concatM (Map [(E (i1,v1))]) (addMap (E e) (Map ms))

concatM :: Map -> Map -> Map 
concatM (Map a) (Map b) = Map (a++b)

removeOverlap :: Inteval -> Interval -> [Intveral] 
removeOverlap (l1, u1) (l2, u2) 
	|u1 < l2 = [(l1,u1),(l2,u2)]
	|(u1 >= l2) && (u1 == u2) = [(l1,l2)]
	|(ul >= l2) && (u1 > u2) =[(l1, 
