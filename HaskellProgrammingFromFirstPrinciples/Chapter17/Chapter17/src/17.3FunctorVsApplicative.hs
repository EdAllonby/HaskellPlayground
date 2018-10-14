module FunctorVsApplicative where

-- You can define a Functor in terms of a provided Applicative instance.
fmap' f x = pure f <*> x

fmapExample = fmap' (+ 1) [1 .. 3]

-- Pure wraps the argument in the applicative
pureList = pure 1 :: [Int]
pureMaybe = pure 1 :: Maybe Int
pureEither = pure 1 :: Either a Int
pureTuple = pure 1 :: ([a], Int)
