module ListComprehensions where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

sqrCubes = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]