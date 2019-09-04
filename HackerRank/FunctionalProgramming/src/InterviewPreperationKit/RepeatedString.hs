{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module RepeatedString where

import           Control.Monad

import           Data.Array
import           Data.Bits
import           Data.List
import           Data.List.Split
import           Data.Set

import           Debug.Trace

import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- Complete the repeatedString function below.
repeatedString :: String -> Int -> Int
repeatedString s n = (maxSeq * length (Data.List.filter ((==) 'a') s))
    + length (Data.List.filter ((==) 'a') (Data.List.take (n - maxSeq * ln) s))
  where
    ln     = length s

    maxSeq = n `div` ln

main :: IO ()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    n <- readLn :: IO Integer

    let result = repeatedString s (fromIntegral n)

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
