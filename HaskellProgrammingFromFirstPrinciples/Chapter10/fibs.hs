fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (<100) fibs

fact :: [Integer]
fact = scanl (*) 1 [2..]

