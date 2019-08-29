module Lib where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State

import           System.Random

someFunc :: IO ()
someFunc = undefined

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

rollDie' :: State StdGen Int
rollDie' = do
    generator <- get
    let (roll, newGenerator) = randomR (1, 6) generator
    put newGenerator
    return roll

rollDice :: State StdGen (Int, Int)
rollDice = liftA2 (,) rollDie rollDie

getDice :: (Int, Int)
getDice = evalState rollDice $ (mkStdGen 1)

rollNDice :: Int -> State StdGen [Int]
rollNDice 0 = return []
rollNDice n = do
    generator <- get
    let (roll, newGen) = randomR (1, 6) generator
    put newGen
    rolls <- rollNDice (n - 1)
    return $ roll : rolls

rollNDice' :: Int -> State StdGen [Int]
rollNDice' n = replicateM n rollDie

myModify :: (s -> s) -> State s ()
myModify f = do
    s <- get
    put $ f s

modifyExample :: State Integer ()
modifyExample = myModify (+ 1)

myGets :: (s -> a) -> State s a
myGets sa = state $ \s -> (sa s, s)