module RandomNumbers where

import           System.Random

sg :: StdGen
sg = mkStdGen 0

nextSg :: (Int, StdGen)
nextSg = next sg

nextNextSg :: (Int, StdGen)
nextNextSg = next $ snd nextSg

nextFromSgTuple :: (Int, StdGen) -> (Int, StdGen)
nextFromSgTuple = next . snd

newSg :: StdGen
newSg = snd (next sg)

newSgIntTupleUsingRandom :: (Int, StdGen)
newSgIntTupleUsingRandom = random newSg

newSgDoubleTupleUsingRandom :: (Double, StdGen)
newSgDoubleTupleUsingRandom = random newSg

randomRange :: (Int, StdGen)
randomRange = randomR (0, 4) newSg

randomRangeN :: Int -> [Int]
randomRangeN n = take n $ randomRs (0, 4) newSg

-- This chaining of state can get tedious. Addressing this tedium is our aim in this chapter.
