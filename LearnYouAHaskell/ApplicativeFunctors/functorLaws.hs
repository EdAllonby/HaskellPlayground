module FunctorLaws where

-- mapping an id function over functor should return the original functor value
-- i.e. the functor should not mutate the value other than what is defined in the function

firstLaw :: (Eq (f b), Functor f) => f b -> Bool
firstLaw x = fmap id x == id x

firstLawPF :: (Eq (f b), Functor f) => f b -> Bool
firstLawPF = (==) <$> fmap id <*> id

-- composing 2 functions should be the same as fmapping twice

secondLaw :: (Eq (f b1), Functor f) => (b2 -> b1) -> (a -> b2) -> f a -> Bool
secondLaw f g = (==) <$> fmap (f . g) <*> fmap f . fmap g

-- breaking the law
data CMaybe a = CNothing | CJust Int a deriving (Eq, Show)

-- Functors should not change anything that's not related to the lift, i.e. parameter 'a'
instance Functor CMaybe where
    fmap _ CNothing    = CNothing
    fmap f (CJust i a) = CJust (i + 1) (f a)

firstLawCMaybeFail :: Bool
firstLawCMaybeFail = firstLawPF (CJust 0 "Hello")

secondLawCMaybeFail :: Bool
secondLawCMaybeFail = secondLaw (+ 1) (* 2) (CJust 0 (1 :: Integer))
