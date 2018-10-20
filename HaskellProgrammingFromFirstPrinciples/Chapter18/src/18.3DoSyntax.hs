module DoSyntax where

import           Control.Monad                  ( join )

-- Applicative also has a sequence operator
sequenceMonadTest = putStrLn "Hello, " >> putStrLn "World"
sequenceApplicativeTest = putStrLn "Hello, " *> putStrLn "World!"

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

-- desugared using monad
sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

-- desugared using applicative
sequencing'' :: IO ()
sequencing'' = putStrLn "blah" *> putStrLn "anothing thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

-- You can't use fmap to apply the monad. This is because the result is wrapped Monads
-- Remember that what's special about bind is that it joins Monads.
fmapGoneWrong :: IO (IO ())
fmapGoneWrong = putStrLn <$> getLine

-- If we join the wrapped monad result, it'll print for us.
fmapFixed :: IO ()
fmapFixed = join fmapGoneWrong

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name please:"
    name <- getLine
    putStrLn ("Hello there, " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = putStrLn "name please:" >> getLine >>= \name ->
    putStrLn ("Hello there, " ++ name)

-- Two bindings makes things even more messy without do notation.
twoBinds :: IO ()
twoBinds = do
    putStrLn "name please:"
    name <- getLine
    putStrLn "age please:"
    age <- getLine
    putStrLn ("Hello there: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' = putStrLn "name please:" >> getLine >>= \name ->
    putStrLn "age please:" >> getLine >>= \age -> putStrLn
        ("Hello there: " ++ name ++ " who is: " ++ age ++ " years old.")

-- You can execute an IO action more than once, and use it like any other type.
printOne = putStrLn "1"
printTwo = putStrLn "2"
twoActions = (printOne, printTwo)
firstAction = fst twoActions
firstActionTwice = firstAction >> firstAction
