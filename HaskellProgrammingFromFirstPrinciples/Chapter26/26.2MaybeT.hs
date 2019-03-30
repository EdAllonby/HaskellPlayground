module MaybeT where

import           Control.Applicative

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure a = MaybeT (pure (pure a))
    (MaybeT f) <*> (MaybeT ma) = let x = (<*>) <$> f <*> ma in MaybeT x


newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second'
    :: [Maybe (Identity a -> Identity b)]
    -> [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final'
    :: [Maybe (Identity a) -> Maybe (Identity b)]
    -> [Maybe (Identity a)]
    -> [Maybe (Identity b)]
final' = (<*>)

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (MaybeT ma) >>= f = MaybeT $ do
        x <- ma
        case x of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
