module NotABurrito where

import           Control.Monad                  ( join
                                                , liftM2
                                                )
import           Control.Applicative            ( liftA2 )

-- fmap can be implemented with a Monad, because all Monads must be Functors
-- The dependency chain: Functor -> Applicative -> Monad
monadMap f xs = xs >>= return . f
monadMapExample = monadMap (+ 2) [1 .. 10]
monadMapExample' = [1 .. 3] >>= return . (+ 2)

-- fmap is like a Monadic bind, but it does not join functors at the end.
-- Here is how you could make an fmap look like a bind, by substituting b with f b
-- fmap :: Functor f => (a -> f b) -> f a -> f (f b)

addOne x = [x, 1]
addOneToTen = addOne 10
addOneBind = fmap addOne [1, 5, 6] -- We have a nested list functor, it is not joined.

-- To make fmap work the same as bind, we need to concat the result at the end
addOneCorrectBind = concat $ fmap addOne [1, 5, 6]
addOneCorrectBind' = concatMap addOne [1, 5, 6] -- Same as using concat $ fmap ...

-- Join is the same as concat, but works for all monads.
joinExample = join [[1 .. 3], [4 .. 6]]

-- Write bind in terms of fmap and join.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- Monads also lift
applicativeLift = liftA2 (,) (Just 3) (Just 5)
monadLift = liftM2 (,) (Just 3) (Just 5)

-- Although zipWith has a similar signature but specialised for lists, their implementations differ.
-- This is because they are using different list monoids.
zipWithLift = zipWith (+) [3, 4] [5, 6]
applicativeListLift = liftM2 (+) [3, 4] [5, 6]
