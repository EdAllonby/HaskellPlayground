{-# LANGUAGE InstanceSigs #-}
module BifunctorExample where

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f  = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving (Show)

instance Bifunctor Deux where
    bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
    bimap f g (Deux a c) = Deux (f a) (g c)

    first :: (a -> b) -> Deux a c -> Deux b c
    first f (Deux a c) = Deux (f a) c

    second :: (b -> c) -> Deux a b -> Deux a c
    second f (Deux a b) = Deux a (f b)

deuxBimapExample :: Deux Integer String
deuxBimapExample = bimap (+ 1) show (Deux 10 20)

newtype Const a b = Const a deriving (Show)

instance Bifunctor Const where
    bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
    bimap f _ (Const a) = Const (f a)

constBimapExample :: Const Integer String
constBimapExample = bimap (+ 10) show (Const 10 :: Const Integer Rational)

data Drei a b c = Drei a b c deriving (Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

dreiBimapExample :: Drei (Integer, Integer) Integer String
dreiBimapExample = bimap (+ 10) show (Drei (1, 2) 10 20.2)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

superDreiExample :: SuperDrei (Int, Int) Integer String
superDreiExample = bimap
    (+ 10)
    show
    (SuperDrei (1, 10) 19 :: SuperDrei (Int, Int) Integer String)

newtype SemiDrei a b c = SemiDrei a deriving (Show)

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

semiDreiExample :: SemiDrei Integer b c
semiDreiExample = id id (SemiDrei 1)

data Quadriceps a b c d = Quadzzz a b c d deriving (Show)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

quadExample :: Quadriceps (Integer, Integer) (Either a Integer) [Char] Rational
quadExample = bimap (++ "!!!") (/ 10) (Quadzzz (1, 20) (Right 10) "Hello" 20)

instance Bifunctor Either where
    bimap f _ (Left  a) = Left (f a)
    bimap _ g (Right b) = Right (g b)

leftExample :: Either Integer Integer
leftExample = bimap (+ 10) (+ 20) (Left 0)

rightExample :: Either Integer Integer
rightExample = bimap (+ 10) (+ 20) (Right 0)
