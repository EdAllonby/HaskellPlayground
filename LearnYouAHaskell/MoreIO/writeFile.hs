import System.IO
import Data.Char

-- This is how withFile could be implemented using bracket.
main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)