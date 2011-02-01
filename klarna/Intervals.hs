module Intervals (newEnv, newStep, findVar) where

import Char (isDigit, isSpace)
import List (delete) 

type UpperBound = Int 
type LowerBound = Int 
type Var = String 
type Entry = (LowerBound, UpperBound, Var) 

data EnvInput = VE Entry | InvalidEntry

data Env = E (Int -> Maybe Var) 

data StepType = OpenLeft | ClosedLeft | OpenBoth | ClosedBoth | Else
data Step = StepOf StepType Entry

--User initializes an enviornment. 
newEnv :: Env
newEnv = E (\n -> Nothing) 

--User adds a new step.  
--Intervals must be intergers. 
--Intervals input as strings.
--Closed intervals are indicated with brackets. 
--Open intervals are indicated with parentheses.
--Variables input as string. 
newStep :: Env -> String -> String -> Env
newStep env interval var 
	= addEnvInput env $ stepToEnvInput $ userInputToStep interval var

--Adds new mapping to enviornment. 
--If invalid entry no change is made to enviornment. 
addEnvInput :: Env -> EnvInput -> Env
addEnvInput (E oldenv) InvalidEntry = E oldenv 
addEnvInput (E oldenv) (VE (l, u, v)) = 
	E (\n -> 
		(if ((n >= l) && (n <= u)) 
			then Just v 
			else oldenv n)) 

--User uses to find variable given a key and an enviornment.
findVar :: Int -> Env -> Maybe Var
findVar n (E e) = e n

--Converts Steps to EnvInputs. 
--If the Intveral is backwards or has no size 
--makes step InvalidEntry. 
--If StepType is Else makes into InvalidEntry.
stepToEnvInput :: Step -> EnvInput 	
stepToEnvInput (StepOf OpenLeft (l,u,v))	
	|(l+1)<=u = (VE (l+1,u,v))
	|otherwise = InvalidEntry
stepToEnvInput (StepOf ClosedLeft (l,u,v))
	|l<=(u-1) = (VE (l,u-1,v))
	|otherwise = InvalidEntry
stepToEnvInput (StepOf OpenBoth (l,u,v))
	|(l+1)<=(u-1) = (VE (l+1,u-1,v))
	|otherwise = InvalidEntry
stepToEnvInput (StepOf ClosedBoth (l,u,v))
	|l<=u = (VE (l,u,v))
	|otherwise = InvalidEntry
stepToEnvInput (StepOf Else _) = InvalidEntry   

--Converts userInput strings to Steps.
--If interval doesn't have the good syntax 
--makes steptype Else. 
userInputToStep :: String -> String -> Step
userInputToStep interval var = (StepOf steptype (lb, ub, var))
	where  
	(steptype, lb, ub)
		| (h=='(') && (l==')') && isDigits = (OpenBoth, lower, upper) 
		| (h=='[') && (l==']') && isDigits = (ClosedBoth, lower, upper)  
		| (h=='(') && (l==']') && isDigits = (OpenLeft, lower, upper)  
	  | (h=='[') && (l==')') && isDigits = (ClosedLeft, lower, upper) 
		| otherwise = (Else, 0, 0) 
	h = head interval 
	l = last interval 
	isDigits =  not (null lBound) && not (null uBound) 
		&&  and (map isDigit lBound) && and (map isDigit uBound)
	lower = read lBound 
	upper = read uBound 
	lBound = delete ' ' (takeWhile (/= ',') (tail interval))
	uBound = delete ' ' (tail (dropWhile (/=',') (init interval)))	
