data Trivial = 
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

data Date =
    Date DayOfWeek Int deriving Show

-- run ':set -Wall' to make sure we don't have any non-exhaustive errors (partial functions)
instance Eq DayOfWeek where
    (==) Mon Mon    = True
    (==) Tue Tue    = True
    (==) Weds Weds  = True
    (==) Thu Thu    = True
    (==) Fri Fri    = True
    (==) Sat Sat    = True
    (==) Sun Sun    = True
    (==) _ _        = False -- comment this one out to see the error.

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') = 
            weekday == weekday'
            && dayOfMonth == dayOfMonth'

-- Non exhaustive example. We should see a compiler warning when -Wall is enabled, because any input other than 2 wil throw an exception.
f :: Int -> Bool
f 2 = True
-- f _ = False

data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'