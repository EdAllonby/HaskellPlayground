k :: (x, y) -> x
k (x, y) = x

k1 :: Num a => a
k1 = k ((4 - 1), 10)

k2 :: String
k2  = k ("three", (1+2))

k3 :: Num a => a
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, b), (c, f))
f (a, b, c) (_, _, f) = ((a, b), (c, f))