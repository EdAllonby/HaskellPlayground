module TopOrLocal where

-- woot is local, if you import the TopOrLocal module elsewhere, users would not be able to reference woot 
topLevelFunction :: Integer -> Integer
topLevelFunction x =
    x + woot + topLevelValue
    where woot :: Integer
          woot = 10

topLevelValue :: Integer
topLevelValue = 5