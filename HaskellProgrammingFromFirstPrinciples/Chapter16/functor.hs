listmap = map (>2) [1..5]

listfmap = fmap (>2) [1..5]

maybemap = fmap (+1) $ Just 2

maybemap2 = (+1) <$> Just 2 -- <$> is the infix alias for fmap

tuplemap = fmap (+1) $ (1,2) -- Just applies to the second value

eithermap = fmap (++ ", bye") $ Right "Hi" -- Just applies to right values

