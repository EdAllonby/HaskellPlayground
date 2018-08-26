import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char

cleanedString = filter (`elem` ['a'..'z']) . map toLower

isPalindrome :: String -> Bool
isPalindrome str = cleanedString str == (reverse . cleanedString) str

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    if isPalindrome line1 
    then putStrLn "It's a palindrome" 
    else do
        putStrLn "Nope"
        exitSuccess