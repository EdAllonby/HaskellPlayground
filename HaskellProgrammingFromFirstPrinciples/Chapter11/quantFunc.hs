data Quantum =
    Yes
  | No
  | Both
    deriving (Eq, Show)

-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes
quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No
quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both
quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes
quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No = Both
quantFlip5 Both = Yes
quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes = No
quantFlip6 No = Yes
quantFlip6 Both = Yes
quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes = Both
quantFlip7 No = Yes
quantFlip7 Both = Yes
quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes = Both
quantFlip8 No = Yes
quantFlip8 Both = No
quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes = Both
quantFlip9 No = No
quantFlip9 Both = No
quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes = Both
quantFlip10 No = No
quantFlip10 Both = Both
quantFlip11 :: Quantum -> Quantum
quantFlip11 Yes = Both
quantFlip11 No = Yes
quantFlip11 Both = Both
-- a few more