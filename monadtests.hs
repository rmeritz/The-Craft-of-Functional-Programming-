--Tackling the Ackward Squad

putLine :: [Char] -> IO()
putLine [] = return()
putLine (c:cs) = putChar c >> putLine cs

--How can I do this using mapM instead?
--putLineM cs = mapM putChar cs 
--I get the type [()] which I don't want. 
--Do I want sequence?

--In do notation 
putLineDo [] = return ()
putLineDO (c:cs) = do {putChar c;
                      putLine cs}


