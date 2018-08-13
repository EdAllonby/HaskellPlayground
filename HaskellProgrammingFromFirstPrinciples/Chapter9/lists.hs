myHead (x : _) = x

myTail [] = []
myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:[]) = Just x

concatExample = [1,2,3] ++ [4,5]

enumFromToExample = enumFromTo 1 10
enumFromToExample' = [1..10]

enumFromThenToExample = enumFromThenTo 1 3 10
enumFromThenToExample' = [1,3..10]

