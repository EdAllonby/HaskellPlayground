module Cipher where
    
import Data.Char
import Data.Bool

caesar _ [] = []
caesar n (x:xs) =  chr shift : caesar n xs
    where shift = base + mod (ord x + n - base) (end - base + 1) 
          base = bool (ord 'a') (ord 'A') (isUpper x)
          end = bool (ord 'z') (ord 'Z') (isUpper x)

unCaesar n xs = caesar (negate n) xs