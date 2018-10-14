module Apl1 where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

first = [1, 2, 3] <> [4, 5, 6]
second = [1, 2, 3] ++ [4, 5, 6]

instance (Monoid a) => Semigroup (ZipList a) where
    (<>) = liftA2 mappend

instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList []

example = ZipList [Sum 1] <> ZipList [Sum 2]

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq

zl = ZipList [1 :: Sum Int]

-- empty ZipList is the zero and not the identity
monoidFail = quickBatch $ monoid zl
