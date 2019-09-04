module ListLength where

len :: [a] -> Int
len = foldr (const (+ 1)) 0