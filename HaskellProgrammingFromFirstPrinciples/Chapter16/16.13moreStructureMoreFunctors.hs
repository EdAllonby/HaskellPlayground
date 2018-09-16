data Wrap f a = Wrap (f a) deriving (Eq, Show) -- This could also be a newtype

instance (Functor f) => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

wrapTest = fmap (+1) (Wrap (Just 1))
