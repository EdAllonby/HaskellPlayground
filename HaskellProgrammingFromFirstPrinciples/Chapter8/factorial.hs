module Factorial where

factorial n 
    | n == 0 = 1
    | otherwise = n * factorial (n - 1) 

factorial' 0 = 1
factorial' n = n * factorial (n - 1)