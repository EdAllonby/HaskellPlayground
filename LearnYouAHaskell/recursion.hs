maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot call maximum' on empty list"
maximum' [x] = x
maximum' (x: xs) = max x (maximum' xs)

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "Cannot call minimum' on empty list"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

replicate' times number
    | times <= 0 = []
    | otherwise = number : replicate' (times - 1) number

take' total items
    | total <= 0 = []
take' _ [] = []
take' total (x:xs) = x : take' (total - 1) xs

-- TODO: Left arrow works to keep guard syntax. Find out what this is called. It looks like list comprehension.
takeUsingLeftArrowSyntax' total items
    | total <= 0 = []
    | [] <- items = []
    | (x:xs) <- items = x : take' (total - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' item = item : repeat' item

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a  -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x  = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

doublefactorial' :: Integer -> Integer
doublefactorial' 0 = 1
doublefactorial' 1 = 1
doublefactorial' n = n * doublefactorial' (n - 2)

-- Just an example of how to do the above non-recursively as an exercise
doublefactorialNonRecursive' :: Integer -> Integer
doublefactorialNonRecursive' n = product [n,n-2..1]