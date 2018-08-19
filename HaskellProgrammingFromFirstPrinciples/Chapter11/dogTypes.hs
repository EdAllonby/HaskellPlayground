data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge

-- 1) Doggies is a type constructor. The data constructors are Husky and Mastiff

-- 2) The kind of Doggies is * -> *. This is because it expects one argument.
--    You can test this by typing ':k Doggies'.

-- 3) The kind of doggies string is just *. It has been fully applied.

-- 4) The type of Husky 10 is 'Num a => Doggies a'. This is because 10 doesn't have to be evaluated as an Int or Integer.

-- 5) The type of Husky (10 :: Integer) is now 'Doggies Integer'. This is because we have explicitly stated the type of 10.

-- 6) The type of Mastiff "Scooby Doo" is 'Doggies [Char]' (Or Doggies String).

-- 7) DogueDeBordeaux is both a type constructor and a data constructor. Although the have the same name, they mean different
--    things in different contexts. Note that this type can become a 'newtype' because it accepts a single type argument.

-- 8) The type of DogueDeBordeaux is 'doge -> DogueDeBordeaux doge'. There are no constraints.

-- 9) The type of DogueDeBordeaux "doggie" is 'DogueDeBordeaux [Char]' (Or DogueDeBordeaux String).