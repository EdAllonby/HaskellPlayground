import Data.Bool

oneBottom = take 1 $ map (+1) [undefined, 2, 3]

twoValue = take 1 $ map (+1) [1, undefined, 3]

threeBottom = take 2 $ map (+1) [1, undefined, 3]

-- Check all chars in a string to see if each char has a vowel
itIsMystery xs = map (\x -> elem x "aeiou") xs

square = map (^2) [1..10]

minOfLists = map minimum [[1..10], [10..20], [20..30]]

sumEachList = map sum [[1..5], [1..5], [1..5]]

alternate = map (\x -> bool (-x) x (x == 3)) [1..10]