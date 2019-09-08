module Bifunctor where

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b
    deriving Show

instance Bifunctor Deux where
    bimap ab cd (Deux a c) = Deux (ab a) (cd c)

data Const a b = Const a

instance Bifunctor Const where
    bimap ab _ (Const a) = Const (ab a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap ab cd (Drei x a c) = Drei x (ab a) (cd c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap ab _ (SuperDrei x a) = SuperDrei x (ab a)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap ce df (Quadzzz a b c d) = Quadzzz a b (ce c) (df d)

data MyEither a b = MyLeft a | MyRight b

instance Bifunctor MyEither where
    bimap ab _ (MyLeft a)  = MyLeft (ab a)
    bimap _ cd (MyRight c) = MyRight (cd c)
