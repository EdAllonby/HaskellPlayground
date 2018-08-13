-- Write your own enumFromTo definitions for the types provided. Do
-- not use range syntax to do so. It should return the same results as if
-- you did [start..stop]. Replace the undefined, an value which results
-- in an error when evaluated, with your own definition.

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool True False = []
eftBool False True = [False, True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd a b
    | a == b = [a]
    | otherwise = []

-- We could also use the following function for the above 2 cases, but will keep them for posterity
eftGeneric :: (Ord a, Enum a) => a -> a -> [a]
eftGeneric first last
        | first > last = []
        | otherwise = first : eftGeneric (succ first) last

eftInt :: Int -> Int -> [Int]
eftInt = eftGeneric

eftChar :: Char -> Char -> [Char]
eftChar = eftGeneric