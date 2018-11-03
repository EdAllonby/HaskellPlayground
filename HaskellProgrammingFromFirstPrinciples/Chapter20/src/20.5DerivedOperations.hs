module DerivedOperations where

import           Data.Monoid
import           Data.Foldable

justToList = toList $ Just 1

justListToList = toList [Just 1, Just 2, Just 3]

justListMappedToList = fmap toList [Just 1, Just 2, Just 3]

justListMappedToListConcat = concat $ fmap toList [Just 1, Just 2, Just 3]

justListMappedToListConcatBetter =
    concatMap toList [Just 1, Just 2, Just 3, Nothing]

-- It's kind of like this. But concatMap actually removes Nothing values (here were just setting Nothing values to 0).
-- This is because concat does [[1], []] is [1] rather than [1, 0]
justListMappedToListMapKindOf = fmap getVal [Just 1, Just 2, Just 3, Nothing]
  where
    getVal Nothing  = 0
    getVal (Just x) = x

-- Null checks if the Foldable has useful data
null1 = null []
null2 = null [1]
null3 = null Nothing
null4 = null $ Just 1
null5 = null (1, Nothing)
null6 = null (1, 1)

nullTest = null [Just 1, Just 2, Nothing]
nullTestMap = fmap null [Just 1, Just 2, Nothing]

length1 = length []
length2 = length [1]
length3 = length Nothing
length4 = length $ Just 2
length5 = length (1, Nothing)
length6 = length (1, 1)
length7 = fmap length [Nothing, Just 1]
length8 = fmap length [Just 1, Nothing]
length9 = fmap length (1, Nothing)
length10 = fmap length (1, Just 10)
length11 = fmap length Just [1 .. 3] -- also works the other way around

elem1 = elem 1 [1]
elem2 = elem 1 [1 .. 3]
elem3 = elem 1 []
elem4 = elem 1 $ Just 2
elem5 = elem 1 $ Just 1 -- It works!
elem6 = elem 1 $ Nothing
elem7 = fmap (elem 3) [Right 1, Right 2, Right 3]
elem8 = (fmap . fmap) (elem 3) [Just $ Right 1, Just $ Right 2, Just $ Right 3]
