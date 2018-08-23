notThe :: String -> Maybe String
notThe x 
    | x == "the" = Nothing
    | otherwise = Just x

replaceThe :: String -> String
replaceThe = go . words
    where go [] = []
          go (x:xs)
            | notThe x == Nothing = "a " ++ go xs 
            | otherwise = x ++ " " ++ go xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . (zip <*> tail) . words
    where startsWithVowel = (`elem` "aeiou") . head
          go [] = 0
          go (x:xs)
            | notThe (fst x) == Nothing && startsWithVowel (snd x) = 1 + go xs
            | otherwise = go xs

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou") 