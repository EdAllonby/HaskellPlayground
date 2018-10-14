module ZipListExercise where

import           Control.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 _           = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) $ fmap f b

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) fs xs = flatMap (<$> xs) fs

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

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys' where
         xs' = let (ZipList' l) = xs
               in take' 3000 l
         ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' (Cons a Nil)
    (<*>) (ZipList' Nil) _ = ZipList' Nil
    (<*>) _ (ZipList' Nil) = ZipList' Nil
    (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = ZipList' $ Cons (f x) ((<*>) fs xs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = fmap ZipList' arbitrary

zipListTests :: IO ()
zipListTests = do
    putStrLn "- ZipList functor tests"
    quickBatch $ functor $ ZipList' (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil)
    putStrLn ""

    putStrLn "- ZipList applicative tests"
    quickBatch $ applicative $ ZipList'
        (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil)
    putStrLn ""
