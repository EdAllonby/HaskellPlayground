-- These could be expressed using fmap
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1)

-- And can be generalised for all functors
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- This can be generalised too
showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- Lifted Exmaples
justInc = liftedInc (Just 1)
listInc = liftedInc [1..5]
rightInc = liftedInc (Right 1)
nothingInc = liftedInc Nothing
emptyInc = liftedInc []

justShow = liftedShow (Just 1)
listShow = liftedShow [1..5]
rightShow = liftedShow (Right 1)
nothingShow = liftedShow (Nothing :: Maybe Int)
emptyShow = liftedShow ([] :: [Int])
