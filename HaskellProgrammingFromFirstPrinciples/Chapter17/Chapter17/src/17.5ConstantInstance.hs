module ConstantInstance where

import           Data.Monoid

--Write an Applicative instance for Constant.
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant $ mappend x y


f = Constant (Sum 1)
g = Constant (Sum 2)
test = f <*> g
test'' = pure 1 :: Constant String Int
test''' = Constant undefined <*> g -- exception
