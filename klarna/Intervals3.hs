module Intervals where

type UB = Int 
type LB = Int 
type Var = String 
type Entry = (LB, UB, Var) 

data Env = E (Int -> Maybe Var) 

newEnv :: Env
newEnv = E (\n -> Nothing) 

addVar :: Env -> Entry -> Env 
addVar (E oldenv) (l, u, v) = 
	E (\n -> 
		(if ((n >= l) && (n <= u)) 
			then Just v 
			else oldenv n)) 

findVar :: Int -> Env -> Maybe Var
findVar n (E e) = e n

data StepType = OpenLeft | ClosedLeft | OpenBoth | ClosedBoth
data Step = S StepType Entry

stepToEntry :: Step -> Entry 	
stepToEntry (S OpenLeft (l,u,v))	
	|(l+1)<=u =(l+1,u,v)
	|otherwise = error "Improper Bounds"
stepToEntry (S ClosedLeft (l,u,v))
	|l<=(u-1) =(l,u-1,v)
	|otherwise = error "Improper Bounds"
stepToEntry (S OpenBoth (l,u,v))
	|(l+1)<=(u-1) =(l+1,u-1,v)
	|otherwise = error "Improper Bounds"
stepToEntry (S ClosedBoth (l,u,v))
	|l<=u = (l,u,v)
	|otherwise = error "Improper Bounds"

