module FizzBuzzDList where

import           Control.Monad.Trans.State
import qualified Data.DList                    as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
    let dList = execState (mapM_ addResult list) DL.empty in DL.apply dList []

-- With snoc, we can apply new results to the end of the list without incurring append costs (O(1))
addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

-- Now we don't need to reverse here
main :: IO ()
main = mapM_ putStrLn $ fizzBuzzList [1 .. 100]
