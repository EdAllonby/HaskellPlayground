module Homework where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x | x <= 0    = []
           | otherwise = digitList x
    where digitList xs = toDigits (xs `div` 10) ++ [xs `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = foldr
    (\x acc -> if (length acc `mod` 2) == 1 then x * 2 : acc else x : acc)
    []

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

-- move n - 1 discs from a to c using b as register
-- move top disc from a to b
-- move n - 1 discs from c to b using a as register
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n > 0 = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
              | otherwise = []

example = hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]

