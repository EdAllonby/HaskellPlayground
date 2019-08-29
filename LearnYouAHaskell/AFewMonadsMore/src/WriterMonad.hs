module WriterMonad where

import           Control.Applicative
import           Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multipleWithLog :: Writer [String] Int
multipleWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return $ a * b

multipleWithLogApp :: Writer [String] Int
multipleWithLogApp = liftA2 (*) (logNumber 3) (logNumber 5)

multipleWithLogAppTell :: Writer [String] Int
multipleWithLogAppTell = liftA2 (*) (logNumber 3) (logNumber 5 <* tell ["multiplying them."])
