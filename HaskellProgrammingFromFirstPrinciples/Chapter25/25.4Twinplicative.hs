{-# LANGUAGE InstanceSigs #-}

module Twinplicative where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

composeFunctorExample :: Compose [] Maybe String
composeFunctorExample = show <$> Compose [Just (1 :: Int)]

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure fgha = Compose $ (pure . pure) fgha
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

pureExample :: Compose [] Maybe String
pureExample = pure "hello"

applyExample :: Compose [] Maybe Integer
applyExample = Compose [Just (+1)] <*> Compose [Just 1]