{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where tooMany :: a -> Bool

instance TooMany Int where tooMany n = n > 42

-- Because of the prgma, we can derive the TooMany class from Int, rather than specifying an instance for Goats
newtype Goats = Goats Int deriving (Eq, Show, TooMany)