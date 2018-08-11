avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | otherwise = 'F' -- This will always be true
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise = False

numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1