import Control.Monad
import Data.Char

main2 = do
    contents <- getContents
    putStrLn $ map toUpper contents

main3 = do
    contents <- getContents
    putStr (shortLinesOnly contents)

main4 = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

main5 = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\line -> if isPalindromeApplicative line then "palindrome" else "not a palindrome") . lines

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs 

isPalindromeApplicative :: String -> Bool
isPalindromeApplicative = (==) <*> reverse 

main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l