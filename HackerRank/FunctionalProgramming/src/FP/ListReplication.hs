{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ListReplication where

main :: IO ()
main = getContents >>= mapM_ print . (\(n : arr) -> f n arr) . map read . words

f :: Int -> [Int] -> [Int]
f n = foldr (\a b -> replicate n a ++ b) []