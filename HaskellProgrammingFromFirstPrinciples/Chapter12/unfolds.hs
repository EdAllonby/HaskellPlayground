import Data.List

-- iterate is like a limited unfold that never ends
iterateExample = take 10 $ iterate (+1) 0

unfoldrExample = take 10 $ unfoldr (\b -> Just (b, b+1)) 0

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where go :: Num a => a -> [a] -> a
          go n [] = n
          go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
    where go :: [a] -> [[a]] -> [a]
          go xs' [] = xs'
          go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f s = s : myIterate f (f s)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = go (f s)
    where go Nothing = []
          go (Just a) = fst a : myUnfoldr f (snd a)
          
    
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))
