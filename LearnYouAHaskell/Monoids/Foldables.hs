module Foldables where

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node x l r) = foldMap f l <> f x <> foldMap f r

tree :: Tree String
tree = Node
  "a"
  (Node "b" EmptyTree EmptyTree)
  (Node "c" (Node "d" EmptyTree EmptyTree) EmptyTree)

treeFold :: String
treeFold = foldMap (++ " ") tree