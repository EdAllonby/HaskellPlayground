module StateExamples where

import           Control.Monad.State

import           System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = let (firstCoin, gen1)  = random gen
                     (secondCoin, gen2) = random gen1
                     (thirdCoin, _)     = random gen2
                 in
                     (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip stack = let ((), newStack1) = push 3 stack
                       (_, newStack2)  = pop newStack1
                   in
                       pop newStack2

popState :: State Stack Int
popState = state pop

pushState :: Int -> State Stack ()
pushState a = state (push a)

-- stackManipState :: Stack -> (Int, Stack)
stackManipState :: State Stack Int
stackManipState = do
    _ <- pushState 3
    _ <- popState
    popState

stackStuff :: State Stack ()
stackStuff = do
    a <- popState
    if a == 5
        then pushState 5
        else do
            pushState 3
            pushState 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManipState
    when (a == 100) stackStuff

stackyStack :: State Stack Stack
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3] then put [8, 3, 1] else put [9, 2, 1]
    return stackNow

randomState :: State StdGen Bool
randomState = state random

-- This makes passing the StdGen around much easier (compared to the threeCoins function above)
threeCoinsState :: State StdGen (Bool, Bool, Bool)
threeCoinsState = do
    a <- randomState
    b <- randomState
    c <- randomState
    return (a, b, c)