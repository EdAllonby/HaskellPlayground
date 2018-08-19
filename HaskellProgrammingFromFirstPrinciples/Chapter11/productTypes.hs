-- This has a cardinality of 3.
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

-- This is a product type. This has a cardinality of 3 * 3 = 9
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

one = MkTwoQs QuantumTrue QuantumTrue
two = MkTwoQs QuantumTrue QuantumFalse
three = MkTwoQs QuantumTrue QuantumBoth
four = MkTwoQs QuantumFalse QuantumFalse
-- ... and so on 5 more times.
-- We could have also written this as (QuantumBool, QuantumBool) i.e. a tuple. Because tuples are also products