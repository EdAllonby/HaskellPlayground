{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\x -> let (a, s) = g x in (f a, s))
