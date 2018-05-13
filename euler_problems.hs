
isThreeOrFive = \x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)

problem_1 = sum (filter isThreeOrFive [1..999])