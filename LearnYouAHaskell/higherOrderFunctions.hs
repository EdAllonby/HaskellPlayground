multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- Create a partially applied function (multTwoWithNine) then apply the remaining 2 arguments.
test = 
    let multTwoWithNine = multThree 9
        in multTwoWithNine 2 3

-- Both compareWithHundred definitions are equivalent. The second one uses partial application.
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- Infix examples with partial application
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Whatever side of the infix is missing will get applied later on. i.e. (10/) is different to (/10)
tenDividedBy :: (Floating a) => a -> a
tenDividedBy = (10/)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Because the minus sign also means negative, haskell requires subract to use the function name rather than '-'. This is a special case.
subtractFour :: (Floating a) => a -> a
subtractFour = subtract 4

-- Need parentheses because -> is naturally right-associative
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwiceExample = applyTwice subtractFour 2
applyTwiceExample' = applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y: ys) = f x y : zipWith' f xs ys

zipWithExample = zipWith' (*) [1..10] [10,9..1]
zipWithExample2 = zipWith' (\a -> \b -> a + b) [1..10] [10,9..1] -- Lambda example
zipWithExample3 = zipWith' subtract (zipWith' (*) [1..10] [10,9..1]) (zipWith' (/) [1..10] [20,19..10]) -- Zip of zips

-- (a -> b -> c) -> (b -> a -> c) is the same as (a -> b -> c) -> b -> a -> c.
-- Which means that both g x y = f y x and f y x = g x y hold true 
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- Old verbose way to do it.
flipOld' :: (a -> b -> c) -> (b -> a -> c)
flipOld' f = g
    where g x y = f y x

flipZipExampleBefore = zip [1..5] "hello"
flipZipExampleAfter = flip' zip [1..5] "hello"
flipZipWithExample = zipWith' (flip' div) (cycle [2]) [10,8..2]