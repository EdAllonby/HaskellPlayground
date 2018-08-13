oneBlowUp = [x^y | x <- [1..5], y <- [2, undefined]]

twoSafe = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

threeBlowUp = sum [1, undefined, 3]

fourSafe = length [1, 2, undefined]

fiveBlowUp = length $ [1, 2, 3] ++ undefined

sixSafe = take 1 $ filter even [1, 2, 3, undefined]

sevenBlowUp = take 1 $ filter even [1, 3, undefined]

eightSafe = take 1 $ filter odd [1, 3, undefined]

nineSafe = take 2 $ filter odd [1, 3, undefined]

tenBlowUp = take 3 $ filter odd [1, 3, undefined]