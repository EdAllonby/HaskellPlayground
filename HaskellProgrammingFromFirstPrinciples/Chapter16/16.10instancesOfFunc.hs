import Test.QuickCheck

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoFC = Two String Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeFC = Three Char String Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

type ThreeFC' = Three' Char Int -> IntToInt -> IntToInt -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four w x y z) = Four w x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourFC = Four (Maybe Int) Char String Int -> IntToInt -> IntToInt -> Bool

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y $ f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

type FourFC' = Four' (Maybe Int) Int -> IntToInt -> IntToInt -> Bool

-- Trivial cannot be implemented because it has a kind of *, and cannot be partially applied to make type * -> *

main = do
    quickCheck (functorCompose' :: IntFC)
    quickCheck (functorCompose' :: IdentityFC)
    quickCheck (functorCompose' :: PairFC)
    quickCheck (functorCompose' :: TwoFC)
    quickCheck (functorCompose' :: ThreeFC)
    quickCheck (functorCompose' :: ThreeFC')
    quickCheck (functorCompose' :: FourFC)
    quickCheck (functorCompose' :: FourFC')
