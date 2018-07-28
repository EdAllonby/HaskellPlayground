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
-- This is because the -> arrow is right-associative.
-- (b -> a -> c) === (b -> (a -> c)) === b -> a -> c
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- Old verbose way to do it.
flipOld' :: (a -> b -> c) -> (b -> a -> c)
flipOld' f = g
    where g x y = f y x

flipZipExampleBefore = zip [1..5] "hello"
-- Flipped version. First is flipped arguments, second is flipped higher order function
flipZipExampleAfterWithoutFlip = zip "hello" [1..5]
flipZipExampleAfter = flip' zip [1..5] "hello"
-- Because of currying, you can also define a flippedZip higher order function which will expect 2 values (called below)
flippedZip = flip' zip
flipZipExampleAfter' = flippedZip [1..5] "hello"

zipWithDivExample = zipWith' div (cycle [2]) [10,8..2]
flipZipWithDivExample = zipWith' (flip' div) (cycle [2]) [10,8..2]

-- map takes a function (a -> b) and an [a] and returns a [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
mapExample = map (2 /) [1..10]
mapNestedExample = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
mapTupleExample = map fst [(1,2),(3,4),(5,6)]

-- filter takes a predicate and list, and returns a list which elements match the predicate
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs -- If the predicate is true, keep it on the list and recursively call the tail
    | otherwise = filter p xs

filterExample = filter (>3) [1,2,3,4,5,4,3,2,1]
filterEvenExample = filter even [1..10]

filterNotInWithLetExample = 
    let notNull x = not (null x) -- This is an alternative to an anonymous function (example below)
    in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

filterNotInWithLambdaExample = (\x -> not (null x)) [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

multipleFilterExample = filter (<15) (filter even [1..20])

quicksortFilterExample :: (Ord a) => [a] -> [a]
quicksortFilterExample [] = []
quicksortFilterExample (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quicksortFilterExample smallerOrEqual ++ [x] ++ quicksortFilterExample larger

-- This uses lazy evaluation.
largestDivisible :: Integer
largestDivisible =  head (filter p [9999999,9999998..])
    where p x = x `mod` 3829 == 0