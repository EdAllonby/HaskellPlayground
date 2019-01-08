module Traverse where

fmapJust :: [Maybe Integer]
fmapJust = fmap Just [1, 2, 3]

-- sequenceA [Just 1, Just 2, Just 3]
sequenceAJust :: Maybe [Integer]
sequenceAJust = sequenceA $ fmap Just [1, 2, 3]

sequenceAJust' :: Maybe [Integer]
sequenceAJust' = sequenceA . fmap Just $ [1, 2, 3]

traverseJust :: Maybe [Integer]
traverseJust = traverse Just [1, 2, 3]

-- MapM is traverse but more restrictive.
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
mapMJust :: Maybe [Integer]
mapMJust = mapM Just [1, 2, 3]

-- traverse is map composed with sequence
t f x = sequenceA ((fmap f) x)
t' f x = (sequenceA . fmap f) x
t'' f = sequenceA . fmap f
