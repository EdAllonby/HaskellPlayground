data Quantum =
    Yes
    | No
    | Both
    deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes
quantSum2 :: Either Quantum Quantum
quantSum2 = Right No
quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both
quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes
quantSum5 :: Either Quantum Quantum
quantSum5 = Left No
quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both