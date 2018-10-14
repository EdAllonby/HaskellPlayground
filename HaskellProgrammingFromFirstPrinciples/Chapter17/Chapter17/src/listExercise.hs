module ListExercise where

import           Control.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

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

f = Cons (+ 1) (Cons (* 2) Nil)
v = Cons 1 (Cons 2 Nil)

test = f <*> v
expected = Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

toMyList = foldr Cons Nil
xs = toMyList [1, 2, 3]
addNineTo x = x `Cons` (9 `Cons` Nil)
test' = flatMap addNineTo xs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [Cons a (Cons b Nil), Nil]

instance Eq a => EqProp (List a) where
    (=-=) = eq

listTests :: IO ()
listTests = do

    putStrLn "- List functor tests"
    quickBatch $ functor (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil)
    putStrLn ""

    putStrLn "- List applicative tests"
    quickBatch $ applicative (Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil)
    putStrLn ""
