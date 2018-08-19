-- All of these have something in common
sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

addOneToThree = foldr (+) 0 [1, 2, 3]

showSumFoldr xs = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" $ map show xs

showSumFoldl xs = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" $ map show xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- Because fold only needs to evaluate the spine in this example, it won't error on the undefined.
undefinedFoldExample = foldr (+) 0 $ take 4 [1, 2, 3, 4, undefined]

scanRSum = scanr (+) 0 [1..5]

scanLSum = scanl (+) 0 [1..5]