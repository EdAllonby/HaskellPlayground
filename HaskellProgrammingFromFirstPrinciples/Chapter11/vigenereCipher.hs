module VigenereCipher where

import Data.Char
import Data.Bool

caesar _ [] = []
caesar n (x:xs) =  chr shift : caesar n xs
    where shift = base + mod (ord x + n - base) (end - base + 1)
          base = bool (ord 'a') (ord 'A') (isUpper x)
          end = bool (ord 'z') (ord 'Z') (isUpper x)

unCaesar n xs = caesar (negate n) xs

getShiftsForString :: String -> String -> String
getShiftsForString secret = snd . foldr (\x acc -> bool (nonSpace x acc) (space x acc) (isSpace x)) (0, "")
    where space x acc = (fst acc, snd acc ++ " ")
          nonSpace x acc = (fst acc + 1, snd acc ++ [cycle secret !! fst acc])
          isSpace x = x == ' '