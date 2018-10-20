module MonadLaws where

-- Identity Laws

-- right identity
-- m >>= return = m

-- left identity
-- return x >>= f = f x

-- Associativity Laws
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

import           Control.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

monadLaws = quickBatch (monad [(1 :: Int, 2 :: Int, 3 :: Int)])
