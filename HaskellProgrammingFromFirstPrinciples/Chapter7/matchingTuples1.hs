module TupleFunctions where

-- These have to be the same type because (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- Not a pattern match
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt a = (fst a) + (snd a)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x