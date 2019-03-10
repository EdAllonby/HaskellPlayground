{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

-- From https://www.youtube.com/watch?v=fCoQb-zqYDI

module WhatIsIOMonad where

import           System.IO.Unsafe
data World = World deriving (Show)

printStr :: String -> World -> World
printStr s !w = unsafePerformIO (putStrLn s >> return w)

readStr :: World -> (String, World)
readStr !w = unsafePerformIO (getLine >>= (\s -> return (s, w)))

type WorldT a = World -> (a, World)

readStrT :: WorldT String
readStrT = readStr

printStrT :: String -> WorldT ()
printStrT s w = ((), printStr s w)

infixl 1 >>>=
(>>>=) :: WorldT a -> (a -> WorldT b) -> WorldT b -- WorldT :: World -> (a, World)
wt >>>= f = uncurry f . wt

whatIsYourPureName :: World -> World
whatIsYourPureName w1 = w4
  where
    w2         = printStr "What is your name?" w1
    (name, w3) = readStr w2
    w4         = printStr ("Hello" ++ name) w3

--Doesn't use World inside function
whatIsYourPureNameT :: WorldT ()
whatIsYourPureNameT = printStrT "What is your name?"
    >>>= \_ -> readStrT >>>= \name -> printStrT ("Hello " ++ name)

newtype WorldM a = WorldM { asT :: WorldT a } deriving (Functor)

instance Applicative WorldM where
    pure x = WorldM $ const (x, _)
    wtf <*> wt = WorldM (asT wtf >>>= \f -> asT wt >>>= \x -> asT $ pure $ f x)

instance Monad WorldM where
    return = pure
    wt >>= f = WorldM (asT wt >>>= asT . f)

printStrM :: String -> WorldM ()
printStrM = WorldM . printStrT

readStrM :: WorldM String
readStrM = WorldM readStrT

whatIsYourPureNameDo :: WorldM ()
whatIsYourPureNameDo = do
    printStrM "What is your name?"
    name <- readStrM
    printStrM $ "Hello" ++ name

whatIsYourNameRun :: ((), World)
whatIsYourNameRun = asT whatIsYourPureNameDo World
