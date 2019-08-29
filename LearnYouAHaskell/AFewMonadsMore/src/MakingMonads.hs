module MakingMonads where

import           Control.Monad

import           Data.List     (all)
import           Data.Ratio

example :: [(Integer, Ratio Integer)]
example = [(3, 1 % 2), (5, 1 % 4), (9, 1 % 2)]

newtype Prob a = Prob { getProp :: [(a, Rational)] }
    deriving Show

propExample :: Prob Integer
propExample = Prob example

instance Functor Prob where
    fmap f (Prob xs) = Prob $ fmap (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
  where
    multAll (Prob innerxs, p) = fmap (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
    pure  = return

    (<*>) = ap

instance Monad Prob where
    return x = Prob [(x, 1 % 1)]

    m >>= f = flatten (fmap f m)

    fail _ = Prob []

data Coin = Heads | Tails
    deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- coin
    return $ all (== Tails) [a, b, c]
