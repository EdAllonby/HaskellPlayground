tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a 
tensDigit' x = d
    where
        (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10
        
hunsD :: Integral a => a -> a 
hunsD x = d
    where
        (xLast, _) = x `divMod` 100
        (_, d) = xLast `divMod` 10

foldBoolPattern :: a -> a -> Bool -> a
foldBoolPattern x _ False = x
foldBoolPattern _ y True = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase a b c = case c of
                    True -> a
                    False -> b 
                    
foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard a b c
    | c = a
    | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c) 