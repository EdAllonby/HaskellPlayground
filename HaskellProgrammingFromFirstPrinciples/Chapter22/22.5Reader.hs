module Reader where

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ \r -> f (ra r) -- or Reader $ (f . ra)

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- fmap is just compose for reader (functions)
composeTest :: Integer
composeTest = compose (+ 2) (* 10) 2 -- 22

fmapTest :: Integer
fmapTest = fmap (+ 2) (* 10) 2 -- 22

ask :: Reader a a
ask = Reader id

askExample :: Int
askExample = runReader ask 10 -- 10
