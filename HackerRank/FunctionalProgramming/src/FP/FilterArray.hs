module FilterArray where

f :: Int -> [Int] -> [Int]
f n = foldr (\a b -> if a < n then a : b else b) []

g :: Int -> [Int] -> [Int]
g n = filter (< n)

main :: IO ()
main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    let numbers = map read (lines inputdata) :: [Int]
    putStrLn . unlines $ (map show . f n) numbers