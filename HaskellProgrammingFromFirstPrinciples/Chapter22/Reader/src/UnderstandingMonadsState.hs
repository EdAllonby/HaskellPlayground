{-# LANGUAGE InstanceSigs #-}

module UnderstandingMonadsState where

import           Control.Applicative
import           Control.Monad
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
import           Control.Monad.Trans.State

import           System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO = flip replicateM (randomRIO (1, 6))

rollNDiceIO2 :: Int -> IO [Int]
rollNDiceIO2 n = forM [1 .. n] $ \_ -> randomRIO (1, 6)

die :: StdGen -> (Int, StdGen)
die = randomR (1, 6)

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
  where
    (n, g) = die (mkStdGen 1)

    (m, _) = die g

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice initialGen = ((n, m), g2)
  where
    (n, g1) = die initialGen

    (m, g2) = die g1

newtype MyState s a = MyState { runMyState :: s -> (a, s) }

instance Functor (MyState s) where
    fmap :: (a -> b) -> MyState s a -> MyState s b
    fmap ab (MyState sas) = MyState $ \s -> let (a, s1) = sas s in (ab a, s1)

instance Applicative (MyState s) where
    pure a = MyState $ \s -> (a, s)

    (<*>) :: (MyState s (a -> b)) -> MyState s a -> MyState s b
    (<*>) (MyState sab) (MyState sa) = MyState $ \s -> let (ab, s1) = sab s in let (a, s2) = sa s1 in (ab a, s2)

instance Monad (MyState s) where
    return :: a -> MyState s a
    return = pure

    p >>= k = MyState $ \s0 -> let (x, s1) = runMyState p s0  -- Running the first processor on s0.
                               in
                                   runMyState (k x) s1         -- Running the second processor on s1.

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)