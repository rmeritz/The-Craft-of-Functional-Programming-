module Intervals (Step , newEnv, newStep, findVar) where

type UpperBound = Int 
type LowerBound = Int 
type Var = String 
type Entry = (LowerBound, UpperBound, Var) 

data EnvInput = VE Entry | InvalidEntry

data Env = E (Int -> Maybe Var) 

data StepType = OpenLeft | ClosedLeft | OpenBoth | ClosedBoth
data Step = Step StepType Entry

newEnv :: Env
newEnv = E (\n -> Nothing) 

newStep :: Env -> Step -> Env
newStep env step = addEnvInput env (stepToEnvInput step)

addEnvInput :: Env -> EnvInput -> Env
addEnvInput (E oldenv) InvalidEntry = E oldenv 
addEnvInput (E oldenv) (VE (l, u, v)) = 
	E (\n -> 
		(if ((n >= l) && (n <= u)) 
			then Just v 
			else oldenv n)) 

findVar :: Int -> Env -> Maybe Var
findVar n (E e) = e n

stepToEnvInput :: Step -> EnvInput 	
stepToEnvInput (Step OpenLeft (l,u,v))	
	|(l+1)<=u = (VE (l+1,u,v))
	|otherwise = InvalidEntry
stepToEnvInput (Step ClosedLeft (l,u,v))
	|l<=(u-1) = (VE (l,u-1,v))
	|otherwise = InvalidEntry
stepToEnvInput (Step OpenBoth (l,u,v))
	|(l+1)<=(u-1) = (VE (l+1,u-1,v))
	|otherwise = InvalidEntry
stepToEnvInput (Step ClosedBoth (l,u,v))
	|l<=u = (VE (l,u,v))
	|otherwise = InvalidEntry

