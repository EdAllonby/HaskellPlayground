module Cow where

import           Control.Applicative

data Cow = Cow { name :: String, age :: Int, weight :: Int } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' = case noEmpty name' of
    Nothing    -> Nothing
    Just nammy -> case noNegative age' of
        Nothing   -> Nothing
        Just agey -> case noNegative weight' of
            Nothing      -> Nothing
            Just weighty -> Just (Cow nammy agey weighty)


cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
    Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')

cow1 :: Maybe (Int -> Int -> Cow)
cow1 = fmap Cow (noEmpty "Bess")

cow2 :: Maybe (Int -> Cow)
cow2 = cow1 <*> noNegative 1

cow3 :: Maybe Cow
cow3 = cow2 <*> Just 10

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply = (<*>)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap
