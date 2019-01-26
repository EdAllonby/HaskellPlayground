{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \x -> (a, x)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (aToB, s' ) = f s
            (a   , s'') = g s'
        in  (aToB a, s'')

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi p) >>= k = Moi $ \s -> let (x, s') = p s in runMoi (k x) s'
    -- The following is invalid because the type will be \s -> Moi s b (notice the \x lambda, substitute x for s.) 
    -- (Moi p) >>= k = \x -> let (a, s) = p x in k a
