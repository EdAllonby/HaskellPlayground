module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
        then putStrLn "You are cool"
    else
        putStrLn "pshh"
    where cool v = 
            v == "coolio"