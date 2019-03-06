module Monoids where

import           Data.Monoid

listAppend :: [Integer]
listAppend = [1, 2, 3] `mappend` [4, 5, 6]

stringAppend :: [Char]
stringAppend = "one" `mappend` "two" `mappend` "three"

listConcat :: [Integer]
listConcat = mconcat [[1, 2], [3, 4], [5, 6]]

productMonoid :: Integer
productMonoid = getProduct $ Product 3 <> Product 9

sumMonoid :: Integer
sumMonoid = getSum $ Sum 10 <> Sum 20

productConcat :: Integer
productConcat = getProduct . mconcat $ Product <$> [1, 2, 3]

anyMonoid :: Bool
anyMonoid = getAny $ Any True <> Any False <> Any False

allMonoid :: Bool
allMonoid = getAll $ All True <> All True <> All False

newtype MyAll = MyAll { getMyAll :: Bool }

instance Semigroup MyAll where
  (<>) (MyAll x) (MyAll y) = MyAll (x && y)

instance Monoid MyAll where
  mempty = MyAll True

myAllMonoid :: Bool
myAllMonoid = getMyAll $ MyAll True <> MyAll True

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ
                       then b
                       else a

-- EQ monoid tales RHS if LHS is EQ. otherwise it'll take the left hand side.
lengthCompareMonoid :: String -> String -> Ordering
lengthCompareMonoid x y = (length x `compare` length y) <> (x `compare` y)

lengthAndVowelsCompareMonoid :: String -> String -> Ordering
lengthAndVowelsCompareMonoid x y = (length x `compare` length y)
  <> (vowels x `compare` vowels y)
  <> (x `compare` y)
  where
    vowels = length . filter (`elem` "aeiou")

maybeMonoid :: Maybe String
maybeMonoid = Just "123" <> Just "456"

firstMaybeMonoid :: Maybe String
firstMaybeMonoid = getFirst
  $ First Nothing <> First (Just "123") <> First (Just "456")

getFirstFromList :: Maybe Integer
getFirstFromList = getFirst . mconcat $ First <$> [Nothing, Just 1, Just 2]

getLastFromList :: Maybe Integer
getLastFromList =
  getLast . mconcat $ Last <$> [Nothing, Just 1, Nothing, Just 2, Nothing]