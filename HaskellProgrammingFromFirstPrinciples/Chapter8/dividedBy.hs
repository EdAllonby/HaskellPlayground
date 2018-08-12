-- Type aliases
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedByBefore :: Numerator -> Denominator -> Quotient
dividedByBefore = div

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)