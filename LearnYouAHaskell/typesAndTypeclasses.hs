removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Adding 'Num' instead of doing Int -> Int -> Int -> Int adds a typeclass constraint so it can work on any type of number, rather than just ints (see addUpToN for Int example)
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addUpToN :: Int -> Int -> Int
addUpToN start total = sum (take total [start..])

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- This returns a type of 'Ordering'
compareExample :: (Ord a) => a -> a -> Ordering
compareExample x y = x `compare` y

-- Something which is an instance of Show can be represented as a string
toString :: (Show a) => a -> String
toString x = show x

-- Read is opposite of Show. A string can be parsed to a type. Note here we need two constraints because we're using read with a num.
negateTwo :: (Num a, Read a) => String -> a
negateTwo x = (read x) - 2

addThreeToStringList :: (Num a, Read a) => String -> [a]
addThreeToStringList x = (read x) ++ [3]

parseInt :: (Num a, Read a) => String -> a
parseInt x = read x 

typeAnnotatedRead = read "(3, 'a')" :: (Int, Char)

typeAnnotatedReadInList = [read "True", False, True, True, False]

successorEnumExample :: Ordering
successorEnumExample = succ LT

lowerBoundTypeExample = minBound :: (Bool, Int, Char)

-- Because we're trying to add an Int to a floating point number, it fails. 'fromIntegral' makes the Int a Num which makes it possible to add.
-- Question: Why doesn't this work? (2::Num) + 3.2 ?
fromIntegralExample = fromIntegral (length [1,2,3,4]) + 3.2