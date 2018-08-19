-- 1) Cardinality of 1
data PugType = PugData

-- 2) Cardinality of 2
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited

-- 3) Cardinality of Int16 is 32768 + 32767 + 1 = 65536

-- 4) Cardinality of Int 9223372036854775808 + 9223372036854775807 + 1 = 2^64 = 18446744073709551616
--    Integer has no max bound, so is its cardinality infinite?

-- 5) 2 ^ n is the cardinality of these number types (Int8 = 2^8, Int = 2^64)