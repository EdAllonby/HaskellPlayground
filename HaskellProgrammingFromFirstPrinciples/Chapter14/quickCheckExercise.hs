import Test.QuickCheck
import Data.List
import Data.Char

half x = x / 2

halfIdentity = (*2) . half

prop_half :: Property
prop_half = forAll (arbitrary :: Gen Float) (\x -> halfIdentity x == x)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_ordered :: Property
prop_ordered = forAll (arbitrary :: Gen [Int]) (listOrdered . sort)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
multiplicationAssociative x y z = x * (y * z) == (x * y) * z
multiplicationCommutative x y = x * y == y * x

genDivisorTuple :: Gen (Integer, Integer)
genDivisorTuple = do
    a <- arbitrary
    b <- suchThat arbitrary (/=0)
    return (a, b)

quotRemRelationship x y = (quot x y)*y + (rem x y) == x
divModRelationship x y = (div x y)*y + (mod x y) == x

reverseTwice :: (Eq a) => [a] -> Bool
reverseTwice x = (reverse . reverse) x == id x

prop_precedence f gen = forAll gen (\x -> (f $ x) == f x)
prop_compose f g gen = forAll gen (\x -> (f . g) x == (\x -> f (g x)) x)

prop_foldedConsAndConcatOperator x y = foldr (:) y x == (++) x y
prop_foldedConcatAndConcat x = foldr (++) [] x == concat x

genTakeTuple :: Gen (Int, [Int])
genTakeTuple = do
    a <- suchThat arbitrary (>0)
    b <- suchThat arbitrary (\x -> length x > a)
    return (a, b)

prop_takeLengthAlwaysSame n xs = length (take n xs) == n

prop_readShowRoundtrip x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord _ = []

capitalizeIdempotence x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

sortIdempotence x = (sort x == twice sort x) && (sort x == fourTimes sort x)

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
    arbitrary = oneof [return Fulse, return Frue]

data Fool2 = Fulse2 | Frue2 deriving (Eq, Show)

instance Arbitrary Fool2 where
    arbitrary = frequency [(2, return Fulse2), (1, return Frue2)]

testAll :: IO ()
testAll = do
    quickCheck prop_half
    quickCheck prop_ordered
    quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative :: Int -> Int -> Bool)
    quickCheck (multiplicationAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (multiplicationCommutative :: Int -> Int -> Bool)
    quickCheck (forAll genDivisorTuple (uncurry quotRemRelationship))
    quickCheck (forAll genDivisorTuple (uncurry divModRelationship))
    quickCheck (forAll (arbitrary :: Gen [Int]) (\x -> reverseTwice x))
    quickCheck (prop_precedence reverse (arbitrary :: Gen [Int]))
    quickCheck (prop_compose (take 1) reverse (arbitrary :: Gen [Int]))
    quickCheck (prop_foldedConsAndConcatOperator :: [Int] -> [Int] -> Bool)
    quickCheck (prop_foldedConcatAndConcat :: [[Int]] -> Bool)
    quickCheck (forAll genTakeTuple (uncurry prop_takeLengthAlwaysSame))
    quickCheck (prop_readShowRoundtrip :: Int -> Bool)
    quickCheck (capitalizeIdempotence :: String -> Bool)
    quickCheck (sortIdempotence :: [String] -> Bool)