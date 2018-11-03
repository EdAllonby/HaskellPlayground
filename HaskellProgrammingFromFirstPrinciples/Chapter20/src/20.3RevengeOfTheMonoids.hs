module RevengeOfTheMonoids where

import           Data.Foldable
import           Data.Monoid

foldrExample = foldr (+) 0 [1 .. 5]

sums = fmap Sum [1 .. 5]
sumsFolded = getSum $ fold sums

sums2 :: [Sum Integer]
sums2 = [1, 2, 3, 4, 5]
sumsFolded2 = getSum $ fold sums2

products = fmap Product [1 .. 5]
productsFolded = getProduct $ fold products

products2 :: [Product Integer]
products2 = [1, 2, 3, 4, 5]
productsFolded2 = getProduct $ fold products2

stringFoldr = foldr (++) "" ["Hello", " Ed"]
stringFoldImplicitMonoid = fold ["Hello", " Ed"]

-- Foldmap takes a function which produces a monoid
-- It uses mappend to fold the values.
sumFoldMap = foldMap Sum [1 .. 5]
productFoldMap = foldMap Product [1 .. 5]
allFoldMap = foldMap All [True, False, True]
anyFoldMap = foldMap Any [True, 10 > 12, 1 == 1, 2 + 2 == 5]
firstFoldMap = foldMap First [Nothing, Just 1, Just 2, Nothing]
lastFoldMap =
    foldMap Last [Nothing, Just 10, Just 12, Nothing, Just 1, Nothing]

lastFoldMap2 = foldMap Last [Nothing, Nothing, Nothing]

-- You can also provide it a function to map that is different from the monoid it is using
-- The following is like ((1+3)*(2+3)*(3+3)*(4+3)*(5+3))
foldMapDifferent = foldMap (+ 3) products
-- The following is like ((1*2)+(2*2)+(3*2)+(4*2)+(5*2))
foldMapDifferent2 = foldMap (* 2) sums

-- Both return the same result in a different monoid
--same as: Sum ((5 * (4 * (3 * (2 * (1 * 3))))))
foldrDifferentMonoid = foldr (*) 3 sums
--same as: Product ((5 * (4 * (3 * (2 * (1 * 3))))))
foldrDifferentMonoid2 = foldr (*) 3 products

-- Both examples below are the same
-- With just 1 value, this is the same as doing 5 * 100
foldMapTimes5 :: Maybe (Product Integer) -> Product Integer
foldMapTimes5 = foldMap (* 5)
foldMapJust = foldMapTimes5 (Just 100) :: Product Integer

-- With just 1 value, this is the same as doing 5 * 100
foldMapTimes5' :: Maybe (Sum Integer) -> Sum Integer
foldMapTimes5' = foldMap (* 5)
foldMapJust2 = foldMapTimes5' (Just 100) :: Sum Integer

-- Mapping nothing returns its identity value
nothingFold = foldMapTimes5 Nothing :: Product Integer
nothingFold2 = foldMapTimes5' Nothing :: Sum Integer
