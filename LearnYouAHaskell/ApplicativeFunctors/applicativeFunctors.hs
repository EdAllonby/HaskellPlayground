module ApplicativeFunctors where

import           Control.Applicative
a :: [Integer -> Integer]
a = fmap (*) [1, 2, 3, 4]

aApplied :: [Integer]
aApplied = fmap (\f -> f 9) a

timesThreeInJust :: Maybe (Integer -> Integer)
timesThreeInJust = fmap (*) (Just 3)

applyingInsideFunctor :: Maybe Integer
applyingInsideFunctor = timesThreeInJust <*> Just 2

-- You can do the above in one line. First the (*) is wrapped in a Just with partially applied 3
-- Then we use <*> to apply it to another Maybe functor to produce Just 6 (Just ((*) 3 2))
applyingOneLiner :: Maybe Integer
applyingOneLiner = (*) <$> Just 3 <*> Just 2

appendHey :: Maybe (String -> String)
appendHey = fmap (++) (Just "hey")

compareA :: Maybe (Char -> Ordering)
compareA = fmap compare (Just 'a')

compareAApplied :: Maybe Ordering
compareAApplied = compareA <*> Just 'b'

functionList :: [Double -> Double -> Double]
functionList = fmap (\x y z -> x + y / z) [3, 4, 5, 6]

functionListApplied :: [Double]
functionListApplied = uncurry <$> functionList <*> [(1, 2)]

-- Apply is similar to fmap, but the function is also inside the context
-- fmap  ::   (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

applicativeJustExample :: Maybe Integer
applicativeJustExample = Just (+ 3) <*> Just 9

applicativeNothingExample :: Maybe Integer
applicativeNothingExample = Just (+ 2) <*> Nothing

applicativeNothingExample2 :: Maybe Integer
applicativeNothingExample2 = Nothing <*> Just (2 :: Integer)

-- Applicative styles - chaining to apply with multiple parameters
-- (<*>) is left associative.
style :: Maybe Integer
style = pure (+) <*> Just 3 <*> Just 2

-- Because pure f <*> x equals f <$> x, we can also do:
style2 :: Maybe Integer
style2 = (+) <$> Just 3 <*> Just 2

-- Applicative also defines a function to encapsulate this well-used pattern.
style3 :: Maybe Integer
style3 = liftA2 (+) (Just 3) (Just 2)

-- List type constructor has an applicative implementation
pureList :: [String]
pureList = pure "Hey" :: [String]

listApplied :: [Integer]
listApplied = [(+ 1), (* 2), (^ 3)] <*> [1 .. 4]

listApplied2 :: [Integer]
listApplied2 = [(*), (^), (+)] <*> [1 .. 5] <*> [5 .. 10]

-- From chapter 1, using list comprehension
findAllValuesOld :: [Integer]
findAllValuesOld = [ x * y | x <- [2, 5, 10], y <- [8, 10, 11] ]
-- Can also be achieved with the applicative style
findAllValuesNew :: [Integer]
findAllValuesNew = (*) <$> [2, 5, 10] <*> [8, 10, 11]

findAllValuesFiltered :: [Integer]
findAllValuesFiltered = filter (> 50) $ (*) <$> [2, 5, 10] <*> [8, 10, 11]

-- IO is also an applicative

myActionOld :: IO String
myActionOld = do
    first  <- getLine
    second <- getLine
    return $ first ++ second

myActionNew :: IO String
myActionNew = (++) <$> getLine <*> getLine

myActionExample :: IO ()
myActionExample = do
    result <- myActionNew
    putStrLn $ "The two concatenated lines are " ++ result

-- Function Applicative
-- (2 * 5) + (3 + 5) == 18
-- 5 is fed into both partially applied functions.
functionApplicative :: Integer
functionApplicative = (+) <$> (* 2) <*> (+ 3) $ 5

functionApplicative2 :: [Double]
functionApplicative2 = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) $ 5

-- List Applicative can also be implemented a different way, but because you can't have different implementations
-- for the same type constructor, a new type is created: ZipList

-- The ZipList applicative implementation uses zipWith.
zipWithExample :: [Integer]
zipWithExample = zipWith (+) [1 .. 5] [1 .. 5]

-- getZipList gets the [] list from the ZipList type
zipListExample :: [Integer]
zipListExample =
    getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100 ..]

zipListExample2 :: [(Char, Char, Char)]
zipListExample2 =
    getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

liftCons :: Maybe [Integer]
liftCons = liftA2 (:) (Just 3) (Just [4])

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA []       = pure []
mySequenceA (x : xs) = liftA2 (:) x (sequenceA xs)

-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])
mySequenceAExample :: Maybe [Integer]
mySequenceAExample = mySequenceA [Just 1, Just 2]

-- Another way to implement sequence
mySequenceA2 :: Applicative f => [f a] -> f [a]
mySequenceA2 = foldr (liftA2 (:)) (pure [])

allMatchingPredicate :: Bool
allMatchingPredicate = and $ map (\f -> f (7 :: Int)) [(> 4), (< 10), odd]

predicateSequence :: Integer -> [Bool]
predicateSequence = sequenceA [(> 4), (< 10), odd]

predicateSequenceApplied :: [Bool]
predicateSequenceApplied = sequenceA [(> 4), (< 10), odd] (7 :: Int)

allMatchingPredicateSequence :: Bool
allMatchingPredicateSequence = and $ sequenceA [(> 4), (< 10), odd] (7 :: Int)

sequencedIOActions :: IO [String]
sequencedIOActions = sequenceA [getLine, getLine, getLine]

ioActionsPrintFirst :: IO ()
ioActionsPrintFirst = do
    (x : _) <- sequencedIOActions
    putStrLn $ "The first thing entered was: " ++ x
