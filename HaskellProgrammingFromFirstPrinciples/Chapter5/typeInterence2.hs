module TypeInference2 where

-- Nothing has changed in this version even though we've omitted the type signature
-- This is because Haskell is clever enough to infer that, using a 'Damas–Hindley–Milner' algorithm
f x y = x + y + 3

-- We can try to work the type inference by hand.
-- First, let's confirm what you said.
-- @let kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- Defined.
-- :t kessel 1 2
-- (Num a, Ord a) => a
-- Cool. Time to backtrack :)
-- :t 1
-- Num p => p
-- Starting with `(Ord a, Num b) => a -> b -> a`, we will infer the first argument of type `a` as `Num p => p`.
-- (Ord a, Num b) => a -> b -> a
-- (Ord p, Num p, Num b) => p -> b -> p
-- a ~ Num p => p, so I essentially substitued all `a`s with `p`s and added the constraints.
-- Though, by doing this application, that actually all goes away.
-- The constraints stays because there's still one `p` for the return type, but our first `p` argument is gone.
-- (Ord p, Num p, Num b) => b -> p
-- Then I'll repeat the same process for the second application, but I'll name it `r` this time.
-- b ~ Num r => r
-- (Ord p, Num p, Num r) => r -> p
-- The application is done, we can remove it.
-- (Ord p, Num p) => p
-- This leaves us with the signature you observed.
-- I'll gladly do other demonstrations if you're interested.
-- (By the way, just nitpicking at myself, I glanced over the constraint during my explanation of the first application, I should've said "of type `Num a => a`" instead of "a" but you should still be able to follow.)
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

kesselApplication = kessel 1 2