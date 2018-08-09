curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)
-- f will be fst, a will be 1, and b will be 2
-- This will create a tuple (a, b) and apply it to f.
curryExample = curry' fst 1 2

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b
uncurryExample = uncurry' (+) (1, 2)

curry3' :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3' f a b c = f (a, b, c)

uncurry3' :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3' f (a, b, c) = f a b c