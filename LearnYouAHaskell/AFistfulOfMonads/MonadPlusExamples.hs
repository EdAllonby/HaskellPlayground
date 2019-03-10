module MonadPlusExamples where

import           Control.Monad

filterComprehension = [x | x <- [1 .. 50], '7' `elem` show x]

-- MonadPlus is for Monads that also act as Monoids.
mplusExample = mplus [1 .. 10] [11 .. 20]

guardExample = [1 .. 50] >>= (\x -> guard ('7' `elem` show x) >> return x)

guardSimple = guard (5 > 2) >> return "cool" :: [String]

guardSimple2 = guard (5 < 2) >> return "cool" :: [String]

guardAsDo = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x