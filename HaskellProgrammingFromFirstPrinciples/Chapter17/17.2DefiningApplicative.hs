class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- fmap
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- applicative
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Same as fmap, but with functions involving more arguments
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d