import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs = trim $ capitaliseStart xs ++ ". " ++ recurse xs
    where capitaliseStart = capitalizeWord . trim . takeWhile (/= '.')
          recurse = capitalizeParagraph . drop 1 . dropWhile (/= '.')

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace