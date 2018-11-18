module SequenceA where

import           Data.Maybe

example1 :: Integer
example1 = sum [1, 2, 3]

example2 :: [Integer]
example2 = fmap sum [Just 1, Just 2, Just 3]

example3 :: Maybe Integer
example3 = (fmap . fmap) sum Just [1, 2, 3]

example4 :: [Integer]
example4 = fmap product [Just 1, Just 2, Nothing]

justNumbers :: [Maybe Integer]
justNumbers = fmap Just [1, 2, 3]

sequenceJustNumbers :: Maybe [Integer]
sequenceJustNumbers = sequenceA justNumbers

justNumbersWithNothing :: [Maybe Integer]
justNumbersWithNothing = [Just 1, Nothing, Just 2]

sequenceJustNumbersWithNothing :: Maybe [Integer]
sequenceJustNumbersWithNothing = sequenceA justNumbersWithNothing

fmapWithSequence :: Maybe Integer
fmapWithSequence = sum <$> sequenceA [Just 1, Just 2, Just 3]

fmapWithNothingSequence :: Maybe Integer
fmapWithNothingSequence = product <$> sequenceA [Just 1, Nothing, Just 3]

catExample :: [Integer]
catExample = catMaybes [Just 1, Nothing, Just 3]

sumCatMaybes :: Integer
sumCatMaybes = sum $ catMaybes [Just 1, Nothing, Just 3]
