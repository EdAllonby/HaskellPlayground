{-# LANGUAGE InstanceSigs #-}

module WriteStateYourself where

newtype State s a = Moi { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (Moi g) = Moi (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = Moi $ \x -> (a, x)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (aToB, s' ) = f s
            (a   , s'') = g s'
        in  (aToB a, s'')

instance Monad (State s) where
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (Moi p) >>= k = Moi $ \s -> let (x, s') = p s in runState (k x) s'
    -- The following is invalid because the type will be \s -> Moi s b (notice the \x lambda, substitute x for s.) 
    -- (Moi p) >>= k = \x -> let (a, s) = p x in k a
