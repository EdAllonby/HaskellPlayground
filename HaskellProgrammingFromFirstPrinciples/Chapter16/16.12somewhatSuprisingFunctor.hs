a = const 1
test = a (Just 10) -- always returns 1

-- This has a phantom type so that we can implement a Functor instance
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

example = const 2 (getConstant (Constant 3))

instance Functor (Constant a) where
    fmap _ (Constant v) = Constant v

-- Because of our implementation, this will return Constant 3 (not Constant 2)
fmapExample = fmap (const 2) (Constant 3)

gc = getConstant
c = Constant 3
first = getConstant $ fmap (const 2) $ Constant 3
second = getConstant $ fmap (const "blah") $ Constant 3
third = fmap (const 2) $ Just 3 -- Here is what we'd expect from any other type of functor.