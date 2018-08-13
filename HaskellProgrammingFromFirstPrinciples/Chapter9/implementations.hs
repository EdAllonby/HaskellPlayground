import Data.Bool

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = (==) x e || myElem e xs

-- Using myAny as the implementation
myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = myAny ((==) e) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myOrderingBy :: (a -> a -> Ordering) -> Ordering -> [a] -> a
myOrderingBy f ord (x:y:[]) = bool y x (f x y == ord)
myOrderingBy f ord (x:y:xs)
    | f x y == ord = myOrderingBy f ord (x:xs)
    | otherwise = myOrderingBy f ord (y:xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = myOrderingBy f GT xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myOrderingBy f LT xs

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs