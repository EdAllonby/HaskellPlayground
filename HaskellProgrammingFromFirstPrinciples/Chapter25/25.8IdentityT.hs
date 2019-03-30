{-# LANGUAGE InstanceSigs #-}

module IdentityT where

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance (Functor f) => Functor (IdentityT f) where
  fmap g (IdentityT fa) = IdentityT $ fmap g fa

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative f) => Applicative (IdentityT f) where
  pure a = IdentityT $ pure a

  (<*>) :: IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b
  (IdentityT faTob) <*> (IdentityT fa) = IdentityT $ faTob <*> fa

instance Monad Identity where
  return = pure

  (Identity a) >>= fab = fab a

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= aToITmb =
    -- runIdentityT ~ IdentityT f a -> f a
    -- aToITmb ~ a -> IdentityT m b
    -- aTomb ~ a -> f b
    -- (>>=) ~ m a -> (a -> m b)
    -- (>>=) ma aTomb ~ m b
    -- Identity $ (>>=) ma aTomb ~ Identity m b
    let aTomb = runIdentityT . aToITmb
    in IdentityT $ ma >>= aTomb

identityMonadTransformerExample :: IdentityT Maybe String
identityMonadTransformerExample = IdentityT (Just (2 :: Integer))
  >>= (\x -> IdentityT (Just $ show $ x + 2))

test = (+ 1)