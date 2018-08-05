module Concat where

-- concat works on a list of any type

simpleConcat = concat [[1,2,3],[2]]
simpleStringConcat = concat ["Iowa", "Bristol", "Cardiff"]
simpleStringAndCharArrayConcat = concat ["Hello", [',', ' ', 'E', 'd']]
concatInner = concat . fmap concat $ [[[1],[2],[3]],[[2]]]