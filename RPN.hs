-- calculator [(Num 3), (Num 2), Add, (Num 4), Mul] -- => 20.0
-- calculator [Number 2]                                    -- => 2.0
-- calculator [(Num 3), (Num 3), Mul, Sqrt]    -- => 3.0
-- calculator [(Num 3), (Num 2), (Num 4), Add, Mul] -- => 18.0

data Entry = Num Float |  Add | Sub | Mul | Div | Sqrt

calculator :: [Entry] -> Float
calculator xs = calc [] xs 
  where
  calc :: [Float] -> [Entry] -> Float
  calc [s] [] = s
  calc (s:ss) [] = (error "Need more operations")
  calc stack ((Num n):es) = calc (n:stack) es
  calc (n:ns) ((Sqrt):es) = calc ((sqrt n) : ns) es	
  calc (n1:n2:ns) ((Add):es) = calc ((n1 + n2) : ns) es
  calc (n1:n2:ns) ((Sub):es) = calc ((n1 - n2) : ns) es
  calc (n1:n2:ns) ((Mul):es) = calc ((n1 * n2) : ns) es
  calc (n1:n2:ns) ((Div):es) = calc ((n1 / n2) : ns) es
  calc _ _ = (error "Need to be more stacked")
  	