module TraverseInstances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data EitherNew a b =
    LeftNew a
    | RightNew b
    deriving (Eq, Ord, Show)

instance Functor (EitherNew a) where
    fmap _ (LeftNew  x) = LeftNew x
    fmap f (RightNew x) = RightNew $ f x

instance Applicative (EitherNew a) where
    pure = RightNew
    LeftNew  e <*> _ = LeftNew e
    RightNew f <*> r = fmap f r

instance Foldable (EitherNew a) where
    foldMap _ (LeftNew  _) = mempty
    foldMap f (RightNew y) = f y

    foldr _ z (LeftNew  _) = z
    foldr f z (RightNew x) = f x z

instance Traversable (EitherNew a) where
    traverse _ (LeftNew  x) = pure (LeftNew x)
    traverse f (RightNew y) = RightNew <$> f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherNew a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ LeftNew a, return $ RightNew b]

instance (Eq a, Eq b) => EqProp (EitherNew a b) where
    (=-=) = eq

main :: IO ()
main = do
    let trigger :: EitherNew Int (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)
