module SafeZipper where

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

(-:) :: a -> (a -> b) -> b
x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a)
    deriving (Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _)       = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _)       = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)
modify _ (Empty, bs)      = Just (Empty, bs)

newFocus :: Maybe (Zipper Char)
newFocus = pure (freeTree, []) >>= goLeft >>= goRight >>= modify (\_ -> 'P')

newFocus2 :: Maybe (Zipper Char)
newFocus2 = newFocus >>= goUp >>= modify (\_ -> 'X')

attach :: Tree a -> Zipper a -> Maybe (Zipper a)
attach t (_, bs) = Just (t, bs)

farLeft :: Maybe (Zipper Char)
farLeft = pure (freeTree, []) >>= goLeft >>= goLeft >>= goLeft

-- newFocusAttach :: Maybe Zipper Char
-- newFocusAttach = (return farLeft) >>= attach (Node 'Z' Empty Empty)
-- topMost :: Zipper a -> Maybe (Zipper a)
-- topMost (t, []) = Just (t, [])
-- topMost z       = topMost (goUp z)
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x : xs, bs) = Just (xs, x : bs)
goForward ([], _)      = Nothing

goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (xs, b : bs) = Just (b : xs, bs)
goBack (_, [])      = Nothing