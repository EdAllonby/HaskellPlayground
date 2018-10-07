identity v = (pure id <*> v) == v
identityExample = identity $ Just 1

composition u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
compositionExample = composition [(+1)] [(*2)] [1..3]

homomorphism :: (Applicative f, Num b) => f b
homomorphism = pure (+1) <*> pure 1

homomorphism' :: (Applicative f, Num b) => f b
homomorphism' = pure ((+1) 1)

interchange u y = (u <*> pure y) == (pure ($ y) <*> u)

interchange1 = Just (+1) <*> Just 1
interchange1' = Just ($ 1) <*> Just (+1) -- Same result

interchange2 = [(+1), (*2)] <*> pure 1
interchange2' = pure ($ 1) <*> [(+1), (*2)]

interchange3 = Just (+3) <*> pure 1
interchange3' = pure ($ 1) <*> Just (+3)