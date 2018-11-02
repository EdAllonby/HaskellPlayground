module ChapterExercises where

import           Data.Monoid
import           Control.Monad                  ( liftM2 )
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Nope Monad
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

-- Flipped Either Monad

data FlippedEither b a = NewLeft a | NewRight b deriving (Eq, Show)

instance Semigroup (FlippedEither b a) where
    NewLeft a <> NewLeft a' = NewLeft a
    NewLeft a <> NewRight b = NewLeft a
    NewRight b <> NewRight b' = NewRight b'
    NewRight b <> NewLeft a = NewLeft a

instance (Monoid b) => Monoid (FlippedEither b a) where
    mempty = NewRight mempty
    mappend = (<>)

instance Functor (FlippedEither b) where
    fmap f (NewLeft a) = NewLeft $ f a
    fmap f (NewRight b) = NewRight b

instance Applicative (FlippedEither b) where
    pure = NewLeft
    (NewRight f) <*> _ = NewRight f
    _ <*> (NewRight x) = NewRight x
    (NewLeft f) <*> (NewLeft x) = NewLeft (f x)

instance Monad (FlippedEither b) where
    NewRight b >>= _ = NewRight b
    NewLeft a >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (FlippedEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ NewLeft b, return $ NewRight a]

instance (Eq b, Eq a) => EqProp (FlippedEither b a) where
    (=-=) = eq

-- Identity Monad

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> a = fmap f a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- List Monad

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) $ fmap f b

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) fs xs = flatMap (<$> xs) fs

instance Monad List where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) (Cons a b) f = f a `append` (b >>= f)

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      frequency [(1, return Nil),
                 (10, return (Cons x y))]

instance Eq a => EqProp (List a) where
    (=-=) = eq

main = do
    let nopeTrigger = undefined :: Nope (Int, String, Int)
        flippedEitherTrigger =
            undefined :: FlippedEither (Int, String, Int) (Int, String, Int)
        identityTrigger = undefined :: Identity (Int, String, Int)
        listTrigger     = undefined :: List (Int, String, Int)
    quickBatch $ functor nopeTrigger
    quickBatch $ applicative nopeTrigger
    quickBatch $ monad nopeTrigger
    quickBatch $ functor flippedEitherTrigger
    quickBatch $ applicative flippedEitherTrigger
    quickBatch $ monad flippedEitherTrigger
    quickBatch $ functor identityTrigger
    quickBatch $ applicative identityTrigger
    quickBatch $ monad identityTrigger
    quickBatch $ functor listTrigger
    quickBatch $ applicative listTrigger
    quickBatch $ monad listTrigger

-- Monad Functions

-- 1
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []       _ = return []
meh (x : xs) f = liftM2 (++) head tail
  where
    head = (: []) <$> f x
    tail = meh xs f

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' = flip traverse

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType x = meh x id

flipType' :: (Monad m) => [m a] -> m [a]
flipType' = sequence

flipTypeTest1 = flipType [Just 1, Just 2, Just 3]
flipTypeTest2 = flipType [Just 1, Nothing, Just 3]
