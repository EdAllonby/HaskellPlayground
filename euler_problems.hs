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

numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

firstChainAboveValue value = head (filter (\x -> length x > value) (map chain [1..]))

-- Unfortunately this is rediculously slow... but works!
problem14 = fst (head ((sortBy (flip compare `on` snd)) (map (\x -> (head x, length x)) (map chain [1..999999]))))