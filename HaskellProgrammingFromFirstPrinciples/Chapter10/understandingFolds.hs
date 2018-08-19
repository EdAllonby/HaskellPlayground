first = foldr (*) 1 [1..5]
first' = foldl (flip (*)) 1 [1..5]
first'' = foldl (*) 1 [1..5]

leftFoldEvaluationSteps = foldl (flip (*)) 1 [1..3]
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs
-- Write out the evaluation steps for
-- foldl (flip (*)) 1 [1..3]
--
-- foldl flip (*) (flip (*) 1 1) [2,3]
-- foldl flip (*) (flip (*) (flip (*) 1 1) 2) [3]
-- foldl flip (*) (flip (*) (flip (*) (flip (*) 1 1) 2) 3) []
-- (flip (*) (flip (*) (flip (*) 1 1) 2) 3)
-- (flip (*) (flip (*) (1 * 1) 2) 3)
-- (flip (*) (2 * (1 * 1)) 3)
-- (3 * (2 * (1 * 1)))
-- (3 * (2 * 1))
-- (3 * 2)
-- 6
