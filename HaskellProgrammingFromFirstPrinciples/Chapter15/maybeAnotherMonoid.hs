import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
    a <- arbitrary
    frequency [(1, return $ First' (Only a)), (1, return $ First' Nada)]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = firstGen

instance Semigroup (First' a) where
    (<>) (First' (Only x)) _ = First' $ Only x 
    (<>) _ (First' (Only x)) = First' $ Only x
    (<>) _ _ = First' Nada

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidAssocTest = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidLeftIdentityTest = quickCheck (monoidLeftIdentity :: String -> Bool)

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
monoidRightIdentityTest = quickCheck (monoidRightIdentity :: String -> Bool)

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
