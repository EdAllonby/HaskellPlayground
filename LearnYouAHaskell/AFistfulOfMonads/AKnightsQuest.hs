module AKnightsQuest where

import           Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [ (c + 2, r - 1)
              , (c + 2, r + 1)
              , (c - 2, r - 1)
              , (c - 2, r + 1)
              , (c + 1, r - 1)
              , (c + 1, r + 2)
              , (c - 1, r - 2)
              , (c - 1, r + 2)]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8]) -- In the bounds of the chess board
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = moveKnight start >>= moveKnight >>= moveKnight

canReactIn3 :: KnightPos -> KnightPos -> Bool
canReactIn3 start end = end `elem` in3' start

getPositions :: KnightPos -> KnightPos -> Maybe [KnightPos]
getPositions = undefined