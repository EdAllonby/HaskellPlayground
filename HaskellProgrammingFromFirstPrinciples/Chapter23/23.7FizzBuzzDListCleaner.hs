module FizzBuzzDListCleaner where

import           Control.Monad.Trans.State
import qualified Data.DList                    as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

-- Because DL.DList String is foldable, we don't need to convert it to a list of strings here.
-- mapM_ in main will correctly fold the DL.DList
fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list = execState (mapM_ addResult list) DL.empty

-- With snoc, we can apply new results to the end of the list without incurring append costs (O(1))
addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

-- Now we don't need to reverse here
main :: IO ()
main = mapM_ putStrLn $ fizzBuzzList [1 .. 100]
