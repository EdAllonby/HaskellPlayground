-- id is maximally polymorphic, or 'parametric'. It accepts all values.
id :: a -> a
id a = a

-- a -> a -> a
f :: a -> a -> a
f a _ = a
f _ b = b

g :: a -> b -> b
g a b = b
