isThreeOrFive = \x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)
problem_1 = sum (filter isThreeOrFive [1..999])

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
problem_2 = sum (takeWhile (<4000000) (filter even fibs))