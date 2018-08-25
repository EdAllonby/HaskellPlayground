lefts' :: [Either a b] -> [a]
lefts' = foldr leftReduce []
    where
        leftReduce (Left x) acc = x : acc
        leftReduce _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr rightReduce []
    where
        rightReduce (Right x) acc = x : acc
        rightReduce _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)