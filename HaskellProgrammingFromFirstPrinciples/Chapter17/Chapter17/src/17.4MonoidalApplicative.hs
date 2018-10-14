module MonoidalApplicative where

import           Data.Monoid

-- [2 * 4, 2 * 5, 3 * 4, 3 * 5]
monoidalExample = [(* 2), (* 3)] <*> [4, 5]

-- applicative version
just = Just (* 2) <*> Just 2
-- functor version
just' = (* 2) <$> Just 2

nothing = Nothing <*> Nothing

fmapIgnoreFirstTupleValue = fmap (+ 1) ("blah", 0)

-- Applicative must have a monoid as the first value
tupleApplicativeExample = (Sum 2, (+ 1)) <*> (Sum 0, 0)
tupleApplicativeExample2 = ("Woo", (+ 1)) <*> (" Hoo!", 2)
tupleApplicativeExample3 = (Product 3, (+ 9)) <*> (Product 2, 8)
tupleApplicativeExample4 = (All True, (+ 1)) <*> (All False, 0)
