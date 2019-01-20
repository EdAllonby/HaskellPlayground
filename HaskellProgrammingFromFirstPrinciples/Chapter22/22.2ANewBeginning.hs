module ANewBeginning where

import           Control.Applicative

boop = (* 2)
doop = (+ 10)

-- Function functor is the same as function composition
bip :: Integer -> Integer
bip = boop . doop

-- fmap boop doop x == (*2) ((+10) x)
bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)

-- So how does that work? What’s happening is we’re feeding a single
-- argument to the (*2) and (+10) and the two results form the two
-- arguments to (+):
-- ((+) <$> (*2) <*> (+10)) 3
-- I still don't fully understand how this works. 

-- Monadic example:
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)
