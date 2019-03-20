module MonadTransformers where

fmapExample :: Maybe Integer
fmapExample = fmap (+ 1) (Just 1)

applicativeExample :: Maybe (Integer, String, [Integer])
applicativeExample = (,,) <$> Just 1 <*> Just "hi" <*> Just [1, 2, 3]

-- Bad transformer
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a = MaybeList { runMaybeList :: [Maybe a] }
-- etc...
