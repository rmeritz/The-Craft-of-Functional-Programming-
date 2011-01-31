module Interval where

import List
import Data.Vector

type Bound = Int 
type IntervalType = String 
type Value = String
type MappingEntry  = (Bound, Bound, IntervalType, Value)
type MappingStored = (Vector Int, Value)  

enumV = Data.Vector.enumFromTo

entryToStored :: MappingEntry -> MappingStored  
entryToStored (lb, ub, it, v) 
	|ub < lb = error "Bounds must be given lower then upper"
	--not quite the correct check 
	|it == "OBE" = (enumV (lb+1) (ub-1), v) 
	|it == "CBE" = (enumV lb ub, v) 
	|it == "CL" = (enumV lb (ub-1), v) 
	|it == "OL" = (enumV (lb+1) ub, v) 
	|otherwise = error "Interval Type must be either OBE, CBE, CL, or OL"

--newStorage :: Vector String
newStorage = empty 

--addEntry :: MappingStored ->  Vector a -> Vector a
addEntry (indexVector, value) oldVector 
	= update_ oldVector indexVector valueVector 
	where
	valueVector = Data.Vector.replicate (Data.Vector.length indexVector) value
