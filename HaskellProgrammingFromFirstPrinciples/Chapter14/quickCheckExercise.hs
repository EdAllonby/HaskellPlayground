import Test.QuickCheck
import Data.List

half x = x / 2

halfIdentity = (*2) . half

prop_half :: Property
prop_half = forAll (arbitrary :: Gen Float) (\x -> halfIdentity x == x)

main :: IO ()
main = quickCheck prop_half
