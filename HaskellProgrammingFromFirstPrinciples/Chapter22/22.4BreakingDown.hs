module BreakingDown where

fmapped :: Integer
fmapped = fmap (+ 1) (* 2) 3

fmapped' :: Integer
fmapped' = fmap (+ 1) (* 2) $ 3

fmapped'' :: Integer
fmapped'' = (fmap (+ 1) (* 2)) 3

-- instance Functor ((->) r) where
--     fmap = (.)

-- fmap :: (b -> c) -> f b -> f c
-- f ~ (-> a)
-- (b -> c) -> (-> a b) -> (-> a c)
-- unroll
-- (b -> c) -> (a -> b) -> (a -> c)
