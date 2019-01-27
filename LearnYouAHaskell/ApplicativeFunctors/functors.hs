module Functors where

import           Data.Char
import           Data.List

-- recap
functorExample :: [Integer]
functorExample = (+ 1) <$> [1, 2, 3]

applicativeExample :: [Integer]
applicativeExample = [(+ 1)] <*> [1, 2, 3]

-- Functors can the thought of adding context to a value, for example Maybe adds a context that the value may not exist (Nothing)
maybefmapExample :: Maybe String
maybefmapExample = fmap (++ "!") (Just "Hi")

maybefmapExample2 :: Maybe String
maybefmapExample2 = fmap (++ "!") Nothing

-- This can now be applied to any functor
abstractedfmapExclaim :: (Functor f) => f String -> f String
abstractedfmapExclaim = fmap (++ "!")

abstractedfmapExclaimExample :: [String]
abstractedfmapExclaimExample = abstractedfmapExclaim ["Hi", "Bye"]

-- IO is also a functor
ioFunctor :: IO Int -> IO Int
ioFunctor = fmap (+ 1)

reverseGetLine :: IO ()
reverseGetLine = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"

reverseGetLineFunctor :: IO ()
reverseGetLineFunctor = do
    reversedLine <- fmap reverse getLine
    putStrLn $ "You said " ++ reversedLine ++ " backwards!"
    putStrLn $ "You definitely said " ++ reversedLine ++ " backwards!"

composed :: IO ()
composed = do
    let composer = intersperse '-' . reverse . fmap toUpper
    line <- getLine
    putStrLn $ composer line

composedFunctor :: IO ()
composedFunctor = do
    line <- fmap (intersperse '-' . reverse . fmap toUpper) getLine
    putStrLn line
