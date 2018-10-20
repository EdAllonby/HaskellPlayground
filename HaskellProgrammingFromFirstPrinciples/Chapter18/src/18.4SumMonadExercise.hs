module SumMonadExercise where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _ = First a
    (<*>) _ (First b) = First b
    (<*>) (Second a) b = a <$> b

instance Monad (Sum a) where
    return = pure
    (>>=) (First a) _ = First a
    (>>=) (Second a) f = f a

sumTest = Second 1 >>= \x -> Second (x + 1) >>= \y -> Second (y + 1)
sumTest2 = First 1 >>= \x -> Second (x + 1) >>= \y -> Second (y + 1)
