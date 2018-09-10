import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc = Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeAssoc = Three (Sum Int) (Product Int) String -> Three (Sum Int) (Product Int) String -> Three (Sum Int) (Product Int) String -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourAssoc = Four (Sum Int) (Product Int) String String -> Four (Sum Int) (Product Int) String String -> Four (Sum Int) (Product Int) String String -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj True) (BoolConj True) = BoolConj True
    (<>) _ _ = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (<>) (BoolDisj True) _ = BoolDisj True
    (<>) _ (BoolDisj True) = BoolDisj True
    (<>) _ _ = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (<>) (Snd a) _ = Snd a
    (<>) _ a = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]

type OrAssoc = Or Int String -> Or Int String -> Or Int String -> Bool

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
      f <- arbitrary
      return $ Combine f

type CombineAssoc = Combine Int (String -> String) -> Combine Int (String -> String) -> Combine Int (String -> String) -> Bool

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    (<>) (Comp f) (Comp g) = Comp (g . f)

data Validation' a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
    (<>) (Success' a) _ = Success' a
    (<>) _ (Success' a) = Success' a
    (<>) (Failure' a) (Failure' b) = Failure' (a <> b)

    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure' a, return $ Success' b]

type ValidationAssoc = Validation' String Int -> Validation' String Int -> Validation' String Int -> Bool


validationCheck = do
    let failure :: String -> Validation' String Int
        failure = Failure'
        success :: Int -> Validation' String Int
        success = Success'
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    -- quickCheck (semigroupAssoc :: CombineAssoc)
    quickCheck (semigroupAssoc :: ValidationAssoc)

