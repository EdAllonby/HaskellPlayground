module VigenereCipher where

import Data.Char
import Data.Bool

shiftChar :: Int -> Char -> Char
shiftChar n x =  chr shift
    where shift = base + mod (ord x + n - base) (end - base + 1)
          base = bool (ord 'a') (ord 'A') (isUpper x)
          end = bool (ord 'z') (ord 'Z') (isUpper x)

vigenereCipher :: String -> String -> String
vigenereCipher secret str = zipWith f str (getShiftsForString secret str)
    where f a b = shiftChar (abs (ord 'A' - ord b)) a -- TODO: Make this work for lowercase and underscore (non alpha chars)

getShiftsForString :: String -> String -> String
getShiftsForString secret = snd . foldr (\x acc -> bool (nonSpace x acc) (space x acc) (isSpace x)) (0, "")
    where space x acc = (fst acc, snd acc ++ " ")
          nonSpace x acc = (fst acc + 1, snd acc ++ [cycle secret !! fst acc])
          isSpace x = x == ' '