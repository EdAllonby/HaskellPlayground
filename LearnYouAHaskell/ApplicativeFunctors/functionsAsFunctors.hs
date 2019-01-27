module FunctionsAsFunctors where

-- (r -> a) can be rewritten as (->) r a, just like (1 + 2) can be rewritten as (+) 1 2

-- substituting f with ((->) r) in the fmap signature
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)

-- This is the same as function composition (:t (.))

functionFunctorExample :: Integer
functionFunctorExample = fmap (+ 1) (* 2) 10

functionFunctorExample2 :: String
functionFunctorExample2 = fmap (show . (+ 1)) (* 2) (10 :: Int)

-- We can rewrite 
-- fmap :: (a -> b) -> f a -> f b
-- as
-- fmap :: (a -> b) -> (f a -> f b)
-- i.e. it takes a function and returns a 'lifted' function

-- Abstracting to a Functor means you can use any context
replicateLifted :: (Functor f) => f Int -> f [Int]
replicateLifted = fmap (replicate 3)

replicateLiftedList :: [[Int]]
replicateLiftedList = replicateLifted [1, 2, 3]

replicateLiftedMaybe :: Maybe [Int]
replicateLiftedMaybe = replicateLifted $ Just 10
