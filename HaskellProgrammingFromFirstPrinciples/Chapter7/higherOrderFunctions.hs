myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f a b = f b a

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

-- (->) is right associative, therefore:
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply aToB a c = aToB a

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
                        GT -> reportBoss e e'
                        EQ -> putStrLn "Neither employee is the boss"
                        LT -> (flip reportBoss) e e' -- This could be written as reportBoss e' e

-- We've made employeeRank2 a HOF by extracting the compare function as an argument.
employeeRank2 :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank2 f e e' = case f e e' of
                        GT -> reportBoss e e'
                        EQ -> putStrLn "Neither employee is the boss"
                        LT -> (flip reportBoss) e e'

employeeRank2Usage = employeeRank2 compare Manager Veep

codersAboveAll :: Employee -> Employee -> Ordering
codersAboveAll Coder Coder = EQ
codersAboveAll Coder _ = GT
codersAboveAll _ Coder = LT
codersAboveAll e e' = compare e e' -- Base case

employeeRank2UsageUpdatedCompare = employeeRank2 codersAboveAll Coder CEO