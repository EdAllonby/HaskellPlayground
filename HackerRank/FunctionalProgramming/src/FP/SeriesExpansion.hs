{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module SeriesExpansion where

import           Control.Monad

import           Data.Array
import           Data.Bits
import           Data.List
import           Data.List.Split

-- import           Data.Set
import           Debug.Trace

import           System.Environment
import           System.IO
import           System.IO.Unsafe

fac :: Int -> Integer
fac = product . flip take [1 ..]

main :: IO ()
main = getContents >>= mapM_ print . map solve . map (read :: String -> Double) . tail . words

e :: Double -> [Double]
e x = [x ** y / product [1 .. y] | y <- [0 ..]]

solve :: Double -> Double
solve x = sum $ take 10 (e x)
