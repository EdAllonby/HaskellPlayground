main = do
     putStrLn "Hello, world"
     let myLine = getLine -- You can give the IO action a different name. This doesn't bind (doesn't execute), it justs gives a new name
     name <- myLine
     putStrLn $ "Hey " ++ name ++ ", you rock!"