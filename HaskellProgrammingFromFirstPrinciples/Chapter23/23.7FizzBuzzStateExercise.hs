module FizzBuzzStateExercise where

import           Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

-- Is this what they are after?
fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo x y | x < y     = fizzBuzzList [y, y - 1 .. x]
                   | otherwise = fizzBuzzList [y .. x]

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100
