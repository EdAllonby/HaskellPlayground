module NumberParser where

import           Control.Applicative

import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = choice (char <$> ['1' .. '9'])

base10Integer :: Parser Integer
base10Integer = do
    negative <- try (char '-') <|> return ' '
    x <- some parseDigit
    return (read $ negative : x)
