module ChapterExercises where

import           Control.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Control.Applicative            ( liftA3 )

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a =  Pair a a
    (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        Pair a <$> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

instance (Monoid a) => Applicative (Two a) where
    pure =  Two mempty
    (<*>) (Two f g) (Two a b) = Two (a `mappend` f) (g b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        Two a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

twoCheck = undefined :: (Two (String, String, String) (String, String, String))

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (<*>) (Three f g h) (Three a b c) = Three (mappend f a) (mappend g b) (h c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        Three a b <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

threeCheck =
    undefined :: ( Three
              (String, String, String)
              (String, String, String)
              (String, String, String)
        )

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
    pure a = Three' mempty a a
    (<*>) (Three' f g h) (Three' a b c) = Three' (mappend f a) (g b) (h c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

threeCheck' =
    undefined :: (Three' (String, String, String) (String, String, String))

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four f g h i) (Four a b c d) = Four (mappend f a) (mappend g b) (mappend h c) (i d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

fourCheck =
    undefined :: ( Four
              (String, String, String)
              (String, String, String)
              (String, String, String)
              (String, String, String)
        )

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
    pure a = Four' mempty mempty mempty a
    (<*>) (Four' f g h i) (Four' a b c d) = Four' (mappend f a) (mappend g b) (mappend h c) (i d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four' a b c <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

fourCheck' =
    undefined :: (Four' (String, String, String) (String, String, String))

chapterTests :: IO ()
chapterTests = do

    putStrLn "- Pair functor tests"
    quickBatch $ functor
        (Pair (1 :: Int, 2 :: Int, 3 :: Int) (1 :: Int, 2 :: Int, 3 :: Int))
    putStrLn ""

    putStrLn "- Pair applicative tests"
    quickBatch $ applicative
        (Pair (1 :: Int, 2 :: Int, 3 :: Int) (1 :: Int, 2 :: Int, 3 :: Int))
    putStrLn ""

    putStrLn "- Two functor tests"
    quickBatch $ functor twoCheck
    putStrLn ""

    putStrLn "- Two applicative tests"
    quickBatch $ applicative twoCheck
    putStrLn ""

    putStrLn "- Three functor tests"
    quickBatch $ functor threeCheck
    putStrLn ""

    putStrLn "- Three applicative tests"
    quickBatch $ applicative threeCheck
    putStrLn ""

    putStrLn "- Three' functor tests"
    quickBatch $ functor threeCheck'
    putStrLn ""

    putStrLn "- Three' applicative tests"
    quickBatch $ applicative threeCheck'
    putStrLn ""

    putStrLn "- Four functor tests"
    quickBatch $ functor fourCheck
    putStrLn ""

    putStrLn "- Four applicative tests"
    quickBatch $ applicative fourCheck
    putStrLn ""

stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
