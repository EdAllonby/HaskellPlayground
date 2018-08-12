-- ⊥ or bottom is a term used in Haskell to refer to computations that do not successfully result in a value
-- In logic, ⊥ corresponds to false.

test = let x = x in x -- On Windows this will freeze GHC.

f :: Bool -> Int
f True = error "blah"
f False = 0

-- This will bottom out because it is a partial function (no match for True inputs)
fPartial :: Bool -> Int
fPartial False = 0

-- data Maybe a = Nothing | Just a
f' :: Bool -> Maybe Int
f' False = Just 0
f' _ = Nothing