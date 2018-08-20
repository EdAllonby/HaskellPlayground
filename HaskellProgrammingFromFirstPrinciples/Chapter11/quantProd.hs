data Quantum =
    Yes
  | No
  | Both
    deriving (Eq, Show)

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)
quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)
quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)
quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)
quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)
quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)
quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)
quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)
quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)