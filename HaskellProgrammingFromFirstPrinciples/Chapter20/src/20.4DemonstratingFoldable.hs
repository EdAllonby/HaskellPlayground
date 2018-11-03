module DemonstratingFoldable where

import           Data.Monoid
import           Data.Foldable

-- Implementing foldable instances

newtype Identity a = Identity a deriving (Show)

-- Because of '{-# MINIMAL foldMap | foldr #-}' Only foldr or foldMap is needed, but here is them all specified.
instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

identityTest = fold $ Identity $ Sum 1
identityTest2 = foldr (*) 1 (Identity 5)
identityTest3 = foldl (*) 5 (Identity 5)
identityTest4 = foldMap (* 5) (Identity $ Sum 1)

data Optional a = Nada | Yep a deriving (Show)

instance Foldable Optional where
    foldr f z Nada = z
    foldr f z (Yep x) = f x z

    -- Again, the below 2 definitions aren't strictly needed because we can derive them from foldr
    foldl f z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep x) = f x
