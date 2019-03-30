module Advent where

import           Control.Monad
import           Debug.Trace

import           Data.List                      ( scanl'
                                                , elemIndex
                                                )
import           Data.IntSet                    ( empty
                                                , insert
                                                , member
                                                , fromList
                                                , IntSet
                                                )


problemOne :: IO Int
problemOne = do
    contents <- readFile "Problem1.txt"
    let allLines = lines contents
    let answer   = foldr calculateNewFrequency 0 allLines
    return answer

problemTwo = do
    contents <- readFile "Problem1.txt"
    let allLines       = lines contents
    let allFrequencies = scanl' (flip calculateNewFrequency) 0 (cycle allLines)
    return $ allFrequencies

firstRepeatedFrequency :: [Int] -> Int
firstRepeatedFrequency allFrequencies = getFreq 1 []
  where
    getFreq i xs = case repeatedFrequency (frequency i) xs of
        Just value -> value
        Nothing    -> trace (show i) (getFreq (i + 1) (frequency i : xs))
    frequency iteration = last $ take iteration allFrequencies

solve2 :: [Int] -> Int
solve2 = go (fromList []) 0 . cycle
  where
    go :: IntSet -> Int -> [Int] -> Int
    go fs f (x : xs) | f `Data.IntSet.member` fs = f
                     | otherwise                 = go (insert f fs) (f + x) xs


repeatedFrequency :: (Foldable t, Eq a) => a -> t a -> Maybe a
repeatedFrequency frequency previousFrequencies =
    if frequency `elem` previousFrequencies then Just frequency else Nothing

calculateNewFrequency :: String -> Int -> Int
calculateNewFrequency = liftM2 calculate head (read . tail)
  where
    calculate '+' f c = c + f
    calculate '-' f c = c - f
    -- A better way to treat an imperfect symbol?
    calculate _   _ _ = 0



parse :: String -> [Int]
parse = map (read . dropWhile (== '+')) . lines

day1a :: String -> Int
day1a = sum . parse

day1b :: String -> Int
day1b input = fst . head . filter (uncurry member) . zip list $ scanl
    (flip insert)
    empty
    list
    where list = scanl' (+) 0 $ cycle $ parse input



day1b' :: String -> Int
day1b' input = fst . head . filter (uncurry member) . zip list $ scanl
    (flip insert)
    empty
    list
  where
    allLines = lines input
    list     = scanl' (flip calculateNewFrequency) 0 (cycle allLines)
