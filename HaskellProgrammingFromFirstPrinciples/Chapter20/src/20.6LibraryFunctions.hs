module LibraryFunctions where

import           Prelude
import           Data.Monoid

sumFoldMap :: (Foldable t, Num a) => t a -> a
sumFoldMap = getSum . foldMap Sum

sumFold :: (Foldable t, Num a) => t a -> a
sumFold = foldr (+) 0

productFoldMap :: (Foldable t, Num a) => t a -> a
productFoldMap = getProduct . foldMap Product

productFold :: (Foldable t, Num a) => t a -> a
productFold = foldr (*) 1

elemFold :: (Foldable t, Eq a) => a -> t a -> Bool
elemFold e = foldr (\x acc -> x == e || acc) False

elemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap e = getAny . foldMap (Any . (e ==))

minimumFold :: (Foldable t, Ord a) => t a -> Maybe a
minimumFold = foldr (applyJust min) Nothing

maximumFold :: (Foldable t, Ord a) => t a -> Maybe a
maximumFold = foldr (applyJust max) Nothing

applyJust :: (a -> a -> a) -> a -> Maybe a -> Maybe a
applyJust f x (Just acc) = Just $ f x acc
applyJust f x _          = Just x

nullFold :: (Foldable t) => t a -> Bool
nullFold = foldr (\_ _ -> False) True

lengthFold :: (Foldable t) => t a -> Int
lengthFold = foldr (\_ acc -> acc + 1) 0

toListFold :: (Foldable t) => t a -> [a]
toListFold = foldr (:) []

foldImplementation :: (Foldable t, Monoid m) => t m -> m
foldImplementation = foldMap (mempty <>)

foldMapImplementation :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapImplementation f = foldr (\x acc -> acc <> f x) mempty
