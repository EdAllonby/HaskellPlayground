import Data.Char

main = do
    line <- getLine
    if null line
        then return ()
        else do
            test <- return "hello"
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words