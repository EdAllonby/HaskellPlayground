mySqr = [x^2 | x <- [1..10]]

first = [x | x <- mySqr, rem x 2 == 0]

second = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

third = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]