module SafeRPN where

import           Control.Monad

solveRPN :: String -> Maybe Double
solveRPN st = do
    result <- foldM foldingFunction [] (words st)
    return $ head result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return $ (x * y) : ys
foldingFunction (x : y : ys) "+" = return $ (x + y) : ys
foldingFunction (x : y : ys) "-" = return $ (x - y) : ys
foldingFunction xs numberString  = fmap (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x, "")] -> Just x
    _         -> Nothing