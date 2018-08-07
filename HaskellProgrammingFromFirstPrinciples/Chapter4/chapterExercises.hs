awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- length :: [a] -> Int
divSafe = 6 `div` (length [1, 2, 3])

-- length [1, 'a', 3, 'b'] won't work because list requires all items of the same type.
length7 = length allAwesome + length awesome
-- (8 == 8) && 9 will not work because '9' isn't truthy.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0 
            then x
          else 
            negate x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

x = (+)
stringLengthPlusOne xs = w `x` 1
                        where w = length xs

identity = \x -> x

first (a, b) = a