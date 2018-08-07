import Data.Tuple

myTup = (1 :: Integer, "blah")

first = fst myTup
second = snd myTup

swapped = swap myTup

firstAddOne = 1 + fst myTup

fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) = b

tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a, b) (c, d) = ((a + c), (b ++ d))