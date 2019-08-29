module AddingLoggingToPrograms where

import           Control.Applicative
import           Control.Monad.Writer

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a 0 = tell ["Finished with: " ++ show a] >> return a
gcd'' a b = do
    let aModB = a `mod` b
    tell [show a ++ " mod " ++ show b ++ " = " ++ show aModB]
    gcd'' b aModB

-- logIt :: IO ()
-- logIt :: [IO ()]
logIt :: Int -> Int -> IO ()
logIt a b = mapM_ putStrLn $ snd $ runWriter (gcd'' a b)

