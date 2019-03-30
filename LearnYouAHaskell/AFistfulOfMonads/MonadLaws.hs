module MonadLaws where

import           Control.Monad

leftIdentity :: Maybe Int
leftIdentity = return 3 >>= \x -> Just (x + 10000)

leftIdentity' :: Maybe Int
leftIdentity' = (\x -> Just (x + 10000)) 3

rightIdentity = Just "move on up" >>= (return)

rightIdentity' = [1, 2, 3, 4] >>= (return)

f :: Num a => a -> [a]
f x = [x, -x]

g :: Num a => a -> [a]
g x = [x * 3, x * 2]

-- (<=<) composes 2 monadic functions (b -> m c) -> (a -> m b) -> (a -> m c)
h :: Integer -> [Integer]
h = f <=< g

branch :: Show a => a -> (IO (), IO ())
branch x = (print x, print x)