class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n
    defaultNumber = Year 1988
    
-- Why not write a typeclass like this? For reasons we’ll explain when
-- we talk about Monoid, it’s important that your typeclasses have laws
-- and rules about how they work. Numberish is a bit... arbitrary. There
-- are better ways to express what it does in Haskell than a typeclass.
-- Functions and values alone suffice here.
defaultAge = defaultNumber :: Age
defaultYear = defaultNumber :: Year