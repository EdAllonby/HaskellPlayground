module MonadicFunctions where

import           Control.Monad.State
import           Control.Monad.Writer

liftMWriter :: (Bool, [Char])
liftMWriter = runWriter $ liftM not $ writer (True, "peas")

-- Functor and Applicative can both be implemented in terms of Monad
fmapAsMonad :: Monad m => (t -> b) -> m t -> m b
fmapAsMonad f xM = do
    x <- xM
    return $ f x

apAsMonad :: Monad m => m (a -> b) -> m a -> m b
apAsMonad fM xM = do
    f <- fM
    x <- xM
    return $ f x

joinMaybe :: Maybe Integer
joinMaybe = join $ Just (Just 10)

joinList :: [Integer]
joinList = join [[1, 2, 3], [4, 5, 6]]

lessThanFour :: [Integer]
lessThanFour = filter (< 4) [1, 2, 3, 4, 5, 6]

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping" ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

keepSmallFilter :: IO ()
keepSmallFilter = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [1, 2, 3, 4, 5, 6]

-- Because lists can be non-deterministic, we can get all sets
powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])

foldLExample :: Integer
foldLExample = foldl (+) 0 [2, 8, 3, 1]

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

foldMExample :: Maybe Int
foldMExample = foldM binSmalls 0 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

logMySums :: Int -> Int -> Writer [String] Int
logMySums acc x = do
    tell ["Adding: " ++ show x ++ " to: " ++ show acc]
    return $ x + acc

foldMWriter :: Writer [String] Int
foldMWriter = foldM logMySums 0 [1, 2, 3, 4, 5]