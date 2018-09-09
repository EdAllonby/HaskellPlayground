import Data.Monoid
import Test.QuickCheck
import Control.Monad

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidAssocTest = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidLeftIdentityTest = quickCheck (monoidLeftIdentity :: String -> Bool)

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
monoidRightIdentityTest = quickCheck (monoidRightIdentity :: String -> Bool)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullAppend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mlr = monoidRightIdentity
    quickCheck (ma :: BullAppend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mlr :: Bull -> Bool)