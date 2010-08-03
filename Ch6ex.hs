--The Craft of Functional Programming 
--Ch. 6 Programming with Lists

import Ch5ex

--6.1

superimposeChar:: Char -> Char -> Char
superimposeChar ch1 ch2 
  |ch1==ch2 &&  ch1=='.'	='.'
  |otherwise			='#'
  
--6.2

superimposeLine:: [Char] -> [Char] -> [Char]
superimposeLine chars1 chars2 = [superimposeChar ch1 ch2 |(ch1, ch2) <- zip chars1 chars2 ] 

--6.3

type Picture = [[Char]]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine chars1 chars2 | (chars1, chars2) <- zip pic1 pic2 ]

--6.4

printPicture :: Picture -> IO()
printPicture pic = putStrLn (onSeperateLines pic)

--6.6

--To get one element of a line of the picture where x is the element number of the line
anElement :: [Char] -> Int -> Char
anElement line x = line!!x

oneLineRotate90 :: Picture -> Int -> [Char]
oneLineRotate90 pic elementofline = reverse [anElement line elementofline | line <- pic ]

rotate90 :: Picture -> Picture 
rotate90 pic = [oneLineRotate90 pic n | n <- [0..(length(pic!!0)-1)] ]

--6.7

antiRotate90 ::Picture -> Picture
antiRotate90 pic = reverse [reverse (oneLineRotate90 pic n) | n <- [0..(length(pic!!0)-1)] ]

--6.8

oneElementScale :: Char -> Int -> [Char]
oneElementScale element scalingnumber = replicate scalingnumber element

oneLineScaleH :: [Char] -> Int -> [Char]
oneLineScaleH line scalingnumber
	|line/=""	= (oneElementScale (head(line)) scalingnumber) ++ (oneLineScaleH (drop 1 line) scalingnumber)
	|otherwise 	=""

oneLineScaleHV :: [Char] -> Int -> Picture
oneLineScaleHV line scalingnumber = --[oneLineScaleH line scalingnumber | n <- [1..scalingnumber]]
	replicate scalingnumber (oneLineScaleH line scalingnumber)

scale :: Picture -> Int -> Picture
scale pic scalingnumber
	| pic/=[] 	=(oneLineScaleHV (head(pic)) scalingnumber) ++ (scale (drop 1 pic) scalingnumber) 
	| otherwise  	=[]

