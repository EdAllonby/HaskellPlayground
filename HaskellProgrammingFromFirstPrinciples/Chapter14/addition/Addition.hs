module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

recursiveMultiply :: (Integral a) => a -> a -> a
recursiveMultiply _ 0 = 0
recursiveMultiply n 1 = n
recursiveMultiply n count = n + recursiveMultiply n (count - 1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

genTreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genEitherExample = sample (genEither :: Gen (Either Int Int))

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing)
             , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $
            2 + 2 `shouldBe` 4
        it "22 divided by 5 is 5 remainder 2" $
            dividedBy 22 5 `shouldBe` (4, 2)
        it "2 * 2 is equal to 4" $
            recursiveMultiply 2 2 `shouldBe` 4
        it "0 * 2 is equal to 0" $
            recursiveMultiply 0 2 `shouldBe` 0
        it "2 * 0 is equal to 0" $
            recursiveMultiply 2 0 `shouldBe` 0
        it "987 * 789 is equal to 778743" $
            recursiveMultiply 987 789 `shouldBe` 778743
        it "x + 1 is always greater than x" $
            property $ \x -> x + 1 > (x :: Int)