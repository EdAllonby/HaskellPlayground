import Data.Char

filterLowercase = filter (isUpper) "HbEfLrLxO"

capitaliseFirst (x:xs) = toUpper x : xs

capitaliseAllRecursive [] = []
capitaliseAllRecursive (x:xs) = toUpper x : capitaliseAllRecursive xs

capitaliseFirstAndReturn xs = toUpper (head xs)

capitaliseFirstAndReturnComposed xs = toUpper . head $ xs

capitaliseFirstAndReturnPF = toUpper . head