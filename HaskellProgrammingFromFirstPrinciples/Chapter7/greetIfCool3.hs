module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  case cool of
    True -> putStrLn "I'm cool"
    False -> putStrLn "pshh"
  where cool = coolness == "coolio"