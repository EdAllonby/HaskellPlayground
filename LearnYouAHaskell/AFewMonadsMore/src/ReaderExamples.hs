module ReaderExamples where

import           Control.Monad.Instances

f :: Integer -> Integer
f = (* 5)

g :: Integer -> Integer
g = (+ 3)

fg :: Integer
fg = fmap f g 8

fgapp :: Integer -> Integer
fgapp = (+) <$> f <*> g

addStuff :: Integer -> Integer
addStuff = do
    a <- f -- arg passed here
    b <- g -- arg also passed here
    return (a + b)