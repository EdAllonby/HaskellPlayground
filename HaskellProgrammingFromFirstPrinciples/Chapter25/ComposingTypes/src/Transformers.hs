{-# LANGUAGE InstanceSigs #-}

module Transformers where

mapping :: Maybe Integer
mapping = fmap (+ 1) (Just 1)

app :: Maybe (Integer, String, [Integer])
app = (,,) <$> Just 1 <*> Just "lol" <*> Just [1, 2]

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT a) = IdentityT $ fmap f a

instance (Applicative m) => Applicative (IdentityT m) where
    pure a = IdentityT $ pure a

    (IdentityT f) <*> (IdentityT a) = IdentityT $ f <*> a

instance Monad Identity where
    return = pure

    (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
    return = pure

    (IdentityT ma) >>= f = IdentityT $ (runIdentityT . f) =<< ma

sumR :: Integer -> IdentityT [] Integer
sumR = return . (+ 1)

example :: IdentityT [] Integer
example = IdentityT [1, 2, 3] >>= sumR

