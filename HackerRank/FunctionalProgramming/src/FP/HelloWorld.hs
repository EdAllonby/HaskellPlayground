{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HelloWorld where

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

main :: IO ()
main = do
    n <- readLn :: IO Int
    replicateM_ n (putStrLn "Hello World")