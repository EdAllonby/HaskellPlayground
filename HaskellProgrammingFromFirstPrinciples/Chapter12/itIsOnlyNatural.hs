data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

natToIntegerExample = natToInteger (Succ (Succ Zero))

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just (go n)
    where 
        go n
            | n == 0 = Zero
            | otherwise = Succ(go (n - 1))
