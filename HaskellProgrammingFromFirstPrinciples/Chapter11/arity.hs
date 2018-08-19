-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- product of Int and String
-- This is similar to a tuple (Int, String). Both are product types.
data Example2 = Example2 Int String deriving (Eq, Show)

-- This type is deriving Eq and Show. Int must implement these.
data MyType = MyVal Int deriving (Eq, Show)