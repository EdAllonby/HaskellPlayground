data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- treeInsert is the folding binary function (takes a tree and a list element and produces a new tree).
-- EmptyTree is the starting accumulator. This function expects a list to be folded over. 
createTree :: (Ord a) => [a] -> Tree a
createTree = foldr treeInsert EmptyTree

-- We can make Tree a Functor (see functor.hs for more examples)
-- This means we can use it with fmap
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

emptyTreeMapExample = fmap (*2) EmptyTree
multiplyTreeBy4 = fmap (*4) . createTree
nonEmptyTreeExample = multiplyTreeBy4 [5,1,2,8,11]