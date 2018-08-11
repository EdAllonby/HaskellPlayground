-- 1
-- These are all equivalent
mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 :: Num a => a -> a -> a -> a
mTh4 = \x -> \y -> \z -> x * y * z

-- 2
-- mTh4 is Num a => a -> a -> a. The type of mTh 3 would be Num a :: a -> a

-- 3
addOne x = x + 1
addOneAnon = \x -> x + 1

-- a
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1

addOneIfOddAnon n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

-- b
addFive x y = (if x > y then y else x) + 5
addFiveAnon = \x -> \y -> (if x > y then y else x) + 5

-- c
mFlip f = \x -> \y -> f y x
mFlipNonAnon f x y = f y x