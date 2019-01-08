module ChapterExercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Traversable Identity where
    sequenceA (Identity a) = Identity <$> a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq


data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldr _ z Nada    = z
    foldr f z (Yep x) = f x z

instance Traversable Optional where
    sequenceA (Yep a) = Yep <$> a
    sequenceA Nada    = pure Nada

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        oneof [return Nada, return $ Yep a]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq



main :: IO ()
main = do
    let trigger :: Identity (Int, Int, [Int])
        trigger = undefined
        optionalTrigger :: Optional (Int, Int, [Int])
        optionalTrigger = undefined
    quickBatch (functor trigger)
    quickBatch (traversable trigger)
    quickBatch (functor optionalTrigger)
    quickBatch (traversable optionalTrigger)
