module Throwdown where

import           System.Random
import           Control.Applicative            ( liftA3 )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Trans.State

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Don't do this normally - use something like Either or Maybe instead
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- It will produce the same results every time, because it is free of effects, but you can make it
-- produce a new result on a new dice roll if you modify the start value.
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s        = mkStdGen 0
        (d1, s1) = randomDieRange s
        (d2, s2) = randomDieRange s1
        (d3, _ ) = randomDieRange s2
    (intToDie d1, intToDie d2, intToDie d3)
    where randomDieRange = randomR (1, 6)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

aRandomDieFromStateExample :: Die
aRandomDieFromStateExample = evalState rollDie $ mkStdGen 0

-- Lifting Int -> Die as it's the last argument of State StdGen
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

rollDieThreeTimesEvaluated :: (Die, Die, Die)
rollDieThreeTimesEvaluated = evalState rollDieThreeTimes' (mkStdGen 0)

-- You can't use repeat.
-- What happened is we repeated a single die value — we didn’t repeat the
-- state action that produces a die. This is what we need:
infiniteDieWrong :: State StdGen [Die]
infiniteDieWrong = repeat <$> rollDie'

infiniteDieWrongEvaluated :: [Die]
infiniteDieWrongEvaluated = take 6 $ evalState infiniteDieWrong (mkStdGen 0)

nDie :: Int -> State StdGen [Die]
nDie = flip replicateM rollDie'

tenDie :: [Die]
tenDie = evalState (nDie 10) (mkStdGen 0)

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go total count gen
        | total >= 20
        = count
        | otherwise
        = let (die, nextGen) = randomR (1, 6) gen
          in  go (total + die) (count + 1) nextGen

rollsToGetTwentyRandomIO :: IO Int
rollsToGetTwentyRandomIO = rollsToGetTwenty . mkStdGen <$> randomIO

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = go 0 0
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go total count n gen
        | total >= n
        = count
        | otherwise
        = let (die, nextGen) = randomR (1, 6) gen
          in  go (total + die) (count + 1) n nextGen

-- I feel like I should be taking advantage of State here...
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0 []
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go currentSum count dies gen
        | currentSum >= n
        = (count, dies)
        | otherwise
        = let (die, nextGen) = randomR (1, 6) gen
          in  go (currentSum + die) (count + 1) (intToDie die : dies) nextGen
