import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- 1)
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf = undefined

-- 2)
capitalizeWords :: String -> [(String, String)]
capitalizeWords = capitalizeSplitWords . words
    where
        capitalizeSplitWords [] = []
        capitalizeSplitWords (word@(firstLetter:remainingLetters):words) = (word, toUpper firstLetter : remainingLetters) : capitalizeSplitWords words