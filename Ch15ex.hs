--The Craft of Functional Programming: Haskell 
--Ch. 15 Case Study: Huffman codes

import Types (Tree(Leaf,Node), Table)

--15.1
{-The effect of export controls can be achieved using import controls. However export controls can can provide qualified names. This high level of controls is provided so that parts of the system that depend on each other can be developed and complied more indepentently. 

--15.2
{-It is the defalt that imported definitions are not themselves imported to prevent the accident editting of a definiation that another part of the system may depend on.-}

--15.3
{- Preventing a module from being exported of imported at all could be useful is the system if code was going known to be going to change. If this was the case it might prevent other modules from breaking when the code in the module breaks. 
This could be achieved in Haskell by having the module hide itself.-}

--15.4
--LRLRRRRRLR

--15.5
--RLLRLRLLRR =>Tree 1 
--at ab t aa
--LRRLRLLRLRLR => Tree 2
--LRLLLRRLLLRLR=> Tree 3
y
--Tree 1 is the most effcient bc the the most commonly used letter is maped to only on movement

--15.6
--battat

--15.7
--RLLRRRRLRL

--15.8

exam1 :: [(Char, Int)]
exam1 = [(a,1),(b,1),(c,1),(a,1),(b,1)]
--goal = [(c,1),(b,2),(a,2)]
--use >= ?
mergeSort :: [(Char, Int)] -> [(Char, Int)]

--15.9

--alpha merge already removed duplicates
--frequency merge should not because that the whole po

--15.10

--Can not use ordering :: a -> a -> Ordering for alphamerge because the ordering is not dependent on the ordering of the alphabet.  

--15.11

insTree :: Tree -> [Tree] -> [Tree]
insTree t [] = [t]
insTree t (t1:ts) 
  | (value t <= value t1)    = t:t1:ts
  | otherwise                = t1 : insTree t ts

--15.12
--see notebook

--15.13

exam2 = (Node 6 (Node 3 (Leaf 'b' 1) (Leaf 'a' 2)) (Leaf 't' 2))
goal:
		(Leaf 'a' 2)
	Node 3
 	 	(Leaf 'b' 1)
Node 6
	(Leaf 'a' 2)

showTree :: Tree -> String 
showTree tr = showTreeTabs tr 0
	where
	showTreeTabs :: Tree -> Int -> String 
	showTreeTabs tr tab
		| tr == (Leaf _ _) = "tr"
		| tr == (Node x t1 t2) =(showTree tab tr  )++ "Node x"   
 

showTable :: Table -> String
