module CiphersInteractive where

import Data.Char
import Data.Bool
import Text.Read

caesar _ [] = []
caesar n (x:xs) =  chr shift : caesar n xs
    where shift = base + mod (ord x + n - base) (end - base + 1) 
          base = bool (ord 'a') (ord 'A') (isUpper x)
          end = bool (ord 'z') (ord 'Z') (isUpper x)

unCaesar n xs = caesar (negate n) xs

main :: IO ()
main = do
    putStrLn "Please input a number to shift."
    shift <- readLn
    putStrLn "Please string to encode."
    str <- readLn
    putStrLn (caesar shift str)