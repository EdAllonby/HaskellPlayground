import Data.Int

-- 1) This cardinality should be 2 + 2 = 4
--    Card of Bool is 2.
--    Card of Big is therefore 2
--    Card of Small is therefore 2.
--    Because this is a sum type, 2 + 2 = 4.
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

-- 2) This cardinality should be 256 + 2 = 258 (Int8 and Bool card respectively)
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)