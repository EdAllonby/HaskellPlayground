import Data.List (sortBy)
import Data.Function (on)

isThreeOrFive = \x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)
problem1 = sum (filter isThreeOrFive [1..999])

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
problem2 = sum (takeWhile (<4000000) (filter even fibs))

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- Problem 14:
-- Unfortunately this is ridiculously slow... but works! Memoization would be the correct way here. Implement with scan?
problem14 = fst . head . sortBy (flip compare `on` snd) . map (listToTupleMap head length) $ map chain [1..999999]
    where listToTupleMap firstFn secondFn xs = (firstFn xs, secondFn xs)
          numLongChains = length (filter isLong (map chain [1..100]))
          isLong xs = length xs > 15