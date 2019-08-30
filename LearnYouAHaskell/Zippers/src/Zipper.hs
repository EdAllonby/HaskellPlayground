module Zipper where

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show

freeTree :: Tree Char
freeTree = Node 'P'
                (Node 'O'
                      (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty))
                      (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)))
                (Node 'L'
                      (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty))
                      (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R
    deriving (Show)

type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L : ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R : ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r)       = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _)       = x

type NaiveBreadcrumbs = [Direction]

naiveGoLeft :: (Tree a, NaiveBreadcrumbs) -> (Tree a, NaiveBreadcrumbs)
naiveGoLeft (Node _ l _, bs) = (l, L : bs)

naiveGoRight :: (Tree a, NaiveBreadcrumbs) -> (Tree a, NaiveBreadcrumbs)
naiveGoRight (Node _ _ r, bs) = (r, R : bs)

(-:) :: a -> (a -> b) -> b
x -: f = f x

ex :: (Tree Char, NaiveBreadcrumbs)
ex = (freeTree, []) -: naiveGoLeft -: naiveGoRight -: naiveGoLeft

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
    deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x r : bs)  = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x l t, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs)      = (Empty, bs)

newFocus :: Zipper Char
newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')

newFocus2 :: Zipper Char
newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

farLeft :: Zipper Char
farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft

newFocusAttach :: Zipper Char
newFocusAttach = farLeft -: attach (Node 'Z' Empty Empty)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z       = topMost (goUp z)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)
