module Lifting where

newtype Identity a = Identity { getIdentity :: a } deriving (Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

identityFunctorExample :: Identity Integer
identityFunctorExample = (+ 1) <$> Identity 2

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

composeFunctorExample :: Compose [] Maybe String
composeFunctorExample = show <$> Compose [Just (1 :: Int)]

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

oneFunctorExample :: One [] String
oneFunctorExample = (++ "!!") <$> One ["ajj"]

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

threeFunctorExample :: Three Maybe [] Maybe Int
threeFunctorExample = (+ 10) <$> (Three $ Just [Just 123])
