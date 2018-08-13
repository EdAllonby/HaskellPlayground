mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

mySqrCubeTuple = [(x, y) | x <- mySqr, y <- myCube]

mySqrCubeTupleLessThanFifty = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

mySqrCubeTupleLessThanN n = [(x, y) | x <- mySqr, y <- myCube, x < n, y < n]

mySqrCubeTupleLessThanFiftyCount = length . mySqrCubeTupleLessThanN $ 50