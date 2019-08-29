module Lib ( someFunc ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test :: String
test = ""

data Foo a = _Foo | Bar a

usedFor :: Num p => Foo p -> p
usedFor Foo     = 1
usedFor (Bar a) = a