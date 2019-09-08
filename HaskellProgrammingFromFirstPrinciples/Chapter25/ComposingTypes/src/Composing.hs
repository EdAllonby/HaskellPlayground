{-# LANGUAGE InstanceSigs #-}

module Composing where

import           Control.Applicative

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity

    (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure

    (>>=) (Identity fga) faIa = faIa fga

newtype Compose f g a = Compose { runCompose :: f (g a) }
    deriving (Eq, Show)

composeExample :: Compose [] Maybe Integer
composeExample = Compose [Just 1, Nothing]

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose a) = Compose $ (fmap . fmap) f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose $ pure <$> pure x

    Compose fs <*> Compose xs   = Compose $ (<*>) <$> fs <*> xs

    -- List Just (a -> b) <*> List Just (a) = List Just b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

newtype One f a = One (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a)))
    deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

liftedTwiceApply
    :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 (a -> b))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
liftedTwiceApply = (liftA2 . liftA2) (<*>)

instance (Applicative f, Applicative g, Applicative h) => Applicative (Three f g h) where
    pure x = Three $ pure . pure <$> pure x

    (Three f) <*> (Three a) = Three $ f `liftedTwiceApply` a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose a) = (foldMap . foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose a) = Compose <$> (traverse . traverse) f a