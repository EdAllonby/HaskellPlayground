module ComposingMonadicFunctions where

import           Control.Monad

f :: Integer -> Integer
f = (+ 1) . (* 100)

g :: Integer -> Maybe Integer
g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))

composeMany :: Integer -> Integer
composeMany = foldr (.) id [(+ 1), (* 100), (+ 1)]