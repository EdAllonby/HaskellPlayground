import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidLeftIdentityTest = quickCheck (monoidLeftIdentity :: String -> Bool)

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
monoidRightIdentityTest = quickCheck (monoidRightIdentity :: String -> Bool)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc = Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Two (Sum Int) (Product Int) -> Bool

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem (\x ->
      let (a, b) = g x
          (c, d) = f b
      in (a <> c, d))

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String (Sum Int) -> Bool)
    quickCheck (monoidRightIdentity :: Two String (Product Int) -> Bool)

f' = Mem $ \s -> ("hi", s + 1)
g' = Mem $ \s -> ("ho", s + 2)
memCheck = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
    print $ runMem (f' <> g') $ 2