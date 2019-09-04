module FilterPositionsInAList where

f :: [Int] -> [Int]
f lst = snd <$> filter (even . fst) (zip [1 ..] lst)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
    inputdata <- getContents
    mapM_ (putStrLn . show) . f . map read . lines $ inputdata