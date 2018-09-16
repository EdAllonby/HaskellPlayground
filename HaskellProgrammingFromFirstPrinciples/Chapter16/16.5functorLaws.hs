-- Functors must follow the following laws
identity :: (Functor f, Eq (f a)) => f a -> Bool
identity x = fmap id x == x

composition = fmap ((+1) . (*2)) [1..5] == (fmap (+1) . fmap (*2) $ [1..5])

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- This instance breaks the identity Functor Law
instance Functor WhoCares where 
    fmap _ ItDoesnt = WhatThisIsCalled -- This breaks the identity law
    fmap _ WhatThisIsCalled = ItDoesnt -- This breaks the identity law
    fmap f (Matter a) = Matter (f a)

test = identity (ItDoesnt :: (WhoCares Int))

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- This instance breaks the composition Functor Law
instance Functor CountingBad where
    fmap f  (Heisenberg n a) = Heisenberg (n+1) (f a)

data CountingGood a = Heisenberg' Int a deriving (Eq, Show)

-- THis instance does not break the composition Functor law
instance Functor CountingGood where
    fmap f (Heisenberg' n a) = Heisenberg' n (f a)