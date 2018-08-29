import Data.Char

putStrRecursive :: String -> IO ()
putStrRecursive [] = return ()
putStrRecursive (x:xs) = do
    putChar x
    putStrRecursive xs

main = do
    putStr "Hey, "
    putStr "I'm "
    putStr "Andy"
    putChar '!'
    putChar ' '
    putStrRecursive "Bye!"
    print 2.222
    print [3,4,3]