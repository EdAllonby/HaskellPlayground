-- If the argument matches the pattern (7) it'll print the match - otherwise it'll go to the catch all 'x' match.
lucky :: Int -> String
lucky 7 = "Lucky Seven"
lucky x = "No Luck"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

-- Recursive pattern match
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Failing pattern match because it's not exhuastive
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bertie"
charName 'c' = "Charlie"

failingCharName :: String
failingCharName = charName 'd'

-- This will print all available charNames using pattern matching
generateAlphabetCharNames :: [String]
generateAlphabetCharNames = fmap charName ['a'..'c']

-- We could manually deconstruct the tuples using fst and snd
addVectorsNonPatternMatch :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectorsNonPatternMatch a b = (fst a + fst b, snd a + snd b)

-- But a better way is to use pattern matching to deconstruct the tuple parts for us.
addVectorsPatternMatch :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectorsPatternMatch (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Implementation of fst, snd and trd for 3-Tuple
-- Underscore (_) is used to denote unused variables in the pattern match.
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- You can also deconstruct using pattern matching inside list comprehension
listComprehensionPatternMatch :: Num a => [(a, a)] -> [a] 
listComprehensionPatternMatch xs = [a+b | (a, b) <- xs] 

-- We can check if a list is empty, and use the x:xs pattern to implement the head function
head' :: [a] -> a
head' [] = error "Empty list cannot be used in head function"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty list"
tell (x:[]) = "the list has one element" ++ show x
tell (x:y:[]) = "The element has two elements: " ++ show x ++ " and: " ++ show y
tell (x:y:_) = "The element has more than two elements. First two are: " ++ show x ++ " and: " ++ show y

-- Another non-exhaustive example. This works with a list of 3 elements, but fails for anything else. There should be another pattern match to match the other cases.
baddAdd :: (Num a) => [a] -> a
baddAdd (x:y:z:[]) = x + y + z

-- The 'as' pattern (@) pattern matches but also keeps the original object structure.
firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]