isItTwo :: Integer -> Bool
-- The order of the matches matters.
-- Try to order from most specific to least.
isItTwo 2 = True
isItTwo _ = False
