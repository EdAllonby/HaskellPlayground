-- https://wiki.haskell.org/99_questions

module HaskellProblems where

-- Problem 1
myLast :: [a] -> a
myLast = foldr1 (const id)

myLast' :: [a] -> a
myLast' = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

myButLast' :: [a] -> a
myButLast' xs = head $ lastButOne (length xs) xs
 where
  lastButOne 2 ys = ys
  lastButOne l ys = lastButOne (l - 1) (tail ys)

myButLast'' :: [a] -> a
myButLast'' xs = xs !! (length xs - 2)

myButLast''' :: [b] -> b
myButLast''' = (!!) <*> (subtract 2 . length)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs x = xs !! (x - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs k = head $ getFrom k xs
 where
  getFrom 1 ys = ys
  getFrom l ys = getFrom (l - 1) (tail ys)

-- Problem 4
myLength :: (Foldable t, Num b) => t a -> b
myLength = foldr (const (+ 1)) 0

myLength' :: (Foldable t, Num a, Functor t) => t b -> a
myLength' = sum . fmap (const 1)

--Problem 5
myReverse :: Foldable t => t a -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == myReverse x

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = (==) <*> myReverse

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- flatten (Elem 5) = [5]
-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) = [1,2,3,4,5]
-- flatten (List []) = []
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a       ) = [a]
flatten (List []      ) = []
flatten (List (a : as)) = flatten a ++ flatten (List as)

-- Problem 8
-- Eliminate consecutive duplicates of list elements.
compress :: String -> String
compress =
  foldr (\x acc -> if null acc || x /= head acc then x : acc else acc) ""
