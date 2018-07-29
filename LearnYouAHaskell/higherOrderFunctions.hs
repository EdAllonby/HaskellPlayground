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

sumOfAllOddSquaresUnderTenThousand = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- By partially applying the multiply function, we can later call an argument to it.
listOfFuns = map (*) [0..]
listOfFunsCall = (listOfFuns !! 4) 5

-- Lambdas are sometimes unneccessary. The following are equivalent
mapWithoutLambda = map (+3) [1,6,3,2]
mapWithLambda = map (\x -> x + 3) [1,6,3,2]

zipWithLambda = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
lambdaPatternMatch = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3)]

curryingExampleFirst x y z = x + y + z
curryingExampleSecond = \x -> \y -> \z -> x + y + z 

sumFolded xs = foldl (\acc x -> acc + x) 0 xs
sumFoldedCurried :: (Num a) => [a] -> a
sumFoldedCurried = foldl (+) 0 -- this is like saying (+ acc x) in each iteration. We don't need to define xs because the function returned expects an xs.

-- Foldr works on infinite lists
mapFolded f xs = foldr (\x acc -> f x : acc) [] xs

elemUsingFold y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- This uses the first value as its starting value. These must not be an empty list.
foldWithoutStartingValue :: (Ord a) => [a] -> a
foldWithoutStartingValue = foldl1 max

reverseUsingFold :: [a] -> [a]
reverseUsingFold = foldl (\acc x -> x : acc) []
-- This can also be done without a lamda, just by flipping the arguments for (:) cons function
reverseUsingFoldWithoutLambda :: [a] -> [a]
reverseUsingFoldWithoutLambda = foldl (flip (:)) []

productUsingFold :: (Num a) => [a] -> a
productUsingFold = foldl (*) 1

filterUsingFold :: (a -> Bool) -> [a] -> [a]
filterUsingFold p = foldr (\x acc -> if p x then x : acc else acc) []

-- Here we're always disgarding the acc from the last pass. So the final pass will set the accumulator the the final element and return.
lastUsingFold :: [a] -> a
lastUsingFold = foldl1 (\_ x -> x)

andWithFold :: [Bool] -> Bool
andWithFold xs = foldr (&&) True xs
andWithFoldInfinite = andWithFold (repeat False) -- Returns false. Doesn't go on forever

-- Scan is similar to fold, but returns all the intermediate values
scanExample = scanl (+) 0 [3,5,2,1]
scanrExample = scanr (+) 0 [3,5,2,1]
scanl1Example = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]

elemUsingScanr y ys = scanr (\x acc -> if x == y then True else acc) False ys
scanlFlip = scanl (flip (:)) [] [3,2,1]

-- How many elements does it take for the sum of the sqaure roots of all natural numebers to exceed 1000?
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

expressionWithoutFunctionApplication = sum (filter (> 10) (map (*2) [2..10]))
expressionWithFunctionApplication = sum $ filter (> 10) $ map (*2) [1..10]

-- You can also treat an application like a function, for example
mappingApplication = map ($ 3) [(4+), (10*), (^2), sqrt]

-- Composition takes the result of one function and passes it into the next function. These functions must match input/output types.
compositionExampleBefore = map (\x -> negate (abs x)) [5, -3, -6, 7, 1, -1]
compositionExampleAfter = map (negate . abs) [5, -3, -6, 7, 1, -1]

compositionExample2Before = map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
compositionExample2After = map (negate . sum . tail) [[1..5], [3..6], [1..7]]

compositionWithMutlipleParametersBefore = sum (replicate 5 (max 6.7 8.9))
compositionWithMutlipleParametersAfter = sum . replicate 5 $ max 6.7 8.9

-- Function application ($) for zipWith, then follow the '(' up and replace with composition (.)
compositionHowToBefore = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
compositionHowToAfter = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

-- Point free style - If you have an argument at the end of a function, you can remove it from both sides (because of currying)
pointFreeStyleBefore xs = foldl (+) 0 xs
pointFreeStyleAfter :: (Num a) => [a] -> a
pointFreeStyleAfter = foldl (+) 0

-- You can't make this a point free style because x is deeply nested. But you can convert it using composition so x is at the end.
pointFreeStyleComplicatedBefore x = ceiling (negate (tan (cos (max 50 x))))
pointFreeStyleComplicatedIntermediate x = ceiling . negate . tan . cos $ max 50 x
pointFreeStyleComplicatedAfter = ceiling . negate . tan . cos . max 50

oddSquareSumCompositionBefore = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSumCompositionAfter = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]