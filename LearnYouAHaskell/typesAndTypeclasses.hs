removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Adding 'Num' instead of doing Int -> Int -> Int -> Int adds a typeclass constraint so it can work on any type of number, rather than just ints (see addUpToN for Int example)
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addUpToN :: Int -> Int -> Int
addUpToN start total = sum (take total [start..])