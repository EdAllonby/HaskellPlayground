import System.IO

-- This is how withFile could be implemented using bracket.
main = do
    contents <- readFile "girlfriend.txt"
    putStr contents