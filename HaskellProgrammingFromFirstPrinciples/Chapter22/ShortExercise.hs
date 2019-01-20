module ShortExercise where

import           Data.Char
import           Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledA :: [Char] -> ([Char], [Char])
tupledA = liftA2 (,) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
    capped <- cap
    revved <- rev
    return (capped, revved)

tupledM2 :: [Char] -> ([Char], [Char])
tupledM2 = cap >>= \x -> rev >>= \y -> return (x, y)
