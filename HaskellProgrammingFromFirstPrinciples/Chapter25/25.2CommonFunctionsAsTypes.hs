module CommonFunctionsAsTypes where

newtype Identity a = Identity { runIdentity :: a }

identityExample :: String
identityExample = runIdentity $ Identity "hello"

-- Compare the type of :t id and kind of :k Identity

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

composeExample :: Compose [] Maybe Int
composeExample = Compose [Just (2 :: Int)]

-- The kind of Compose is the same as the type of (.)
-- f ~ []
-- g ~ Maybe
-- a ~ Int
