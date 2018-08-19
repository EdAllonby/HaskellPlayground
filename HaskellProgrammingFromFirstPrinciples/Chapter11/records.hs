-- Simple product type
data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108

ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

-- Try :t name. Record types just create helper functions.
-- :t name is Person' -> String 
data Person' =
    Person' { name :: String
    , age :: Int }
    deriving (Eq, Show)

jm' = Person' "julie" 108

ca' = Person' "chris" 16

caName = name ca'