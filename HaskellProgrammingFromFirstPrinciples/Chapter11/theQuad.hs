data Quad =
    One
  | Two
  | Three
  | Four deriving (Eq, Show)

-- eQuad :: Either Quad Quad
-- This takes 4 + 4 = 8

-- prodQuad :: (Quad, Quad)
-- This takes 4 * 4 = 16

-- funcQuad :: Quad -> Quad
-- this takes 4 ^ 4 = 256

-- prodTBool :: (Bool, Bool, Bool)
-- this takes 2 * 2 * 2 = 8

-- gTwo :: Bool -> Bool -> Bool
-- this takes 2 ^ 2 ^ 2 = 16

-- fTwo :: Bool -> Quad -> Quad
-- (c ^ b) ^ a = (4 ^ 4) ^ 2 = 65536