-- Unary constructors are the identity function.

-- What goes in, goes out. Any valid Int can also be used for Goats.
data GoatsOld = GoatsOld Int deriving (Eq, Show)

-- Any unary data constructor can be defined as a newtype.
-- Some advantages over a vanilla data declaration.
-- No runtime overhead as it reuses the representation of the type it contains.
newtype Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Cows = Cows Int deriving (Eq, Show)

-- We can pattern match to make this function safer.
tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43