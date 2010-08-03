--Use Pictures

--Exercises 2.1 thru 2.1
--2.1

import Pictures

blackHorse :: Picture 
blackHorse = invertColour (horse)

rotateHorse :: Picture
rotateHorse = rotate horse


--2.2

black :: Picture
black = superimpose horse (blackHorse)

--2.3

checkerBoard2by2 :: Picture
checkerBoard2by2 = above (sideBySide white (black))(sideBySide black (white))

checkerBoard4by4 :: Picture
checkerBoard4by4 = above (sideBySide checkerBoard2by2 (checkerBoard2by2)) (sideBySide checkerBoard2by2 (checkerBoard2by2))

checkerBoard8by8 :: Picture
checkerBoard8by8 = sideBySide checkerBoard4by4 (checkerBoard4by4)

--2.4

horsevar1 :: Picture
horsevar1 = above (sideBySide horse (blackHorse))(sideBySide blackHorse (horse))

horsevar2 :: Picture
horsevar2 = above (sideBySide horse (blackHorse))(sideBySide (flipV (blackHorse)) (flipV(horse)))

horsevar3 :: Picture
horsevar3 = above (sideBySide horse (blackHorse))(sideBySide (rotate (blackHorse)) (rotate (horse)))

--2.5

horsevar4 :: Picture
horsevar4 = above (sideBySide horse (blackHorse))(sideBySide (flipH (blackHorse)) (flipH(horse)))
