-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs)
    | isNothing x = catMaybes xs
    | otherwise = getValue x : catMaybes xs
    where getValue (Just x) = x


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = case compare (length ys) (length xs) of
                    EQ -> Just ys
                    _ -> Nothing
    where ys = catMaybes xs