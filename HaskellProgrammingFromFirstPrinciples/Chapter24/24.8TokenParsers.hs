module TokenParsers where

import           Control.Applicative
import           Text.Trifecta

parseExample :: Result String
parseExample = parseString (some digit) mempty "123 456"

parseExample2 = parseString (some (some digit)) mempty "123 456"

parseExample3 = parseString (some integer) mempty "123"

parseExample4 = parseString (some integer) mempty "123456"

parseExampleWithSpace = parseString (some integer) mempty "123 456"

parseExampleWithNewline = parseString (some integer) mempty "123\n\n\n456"

-- This won't do as you expect
parseToken = parseString (token (some digit)) mempty "123\n\n456"

parseToken2 = parseString (token (some (token digit))) mempty "123\n\n456"

p' :: Parser [Integer]
p' = some $ do
    i <- token (some digit)
    return (read i)

s = "1\n2\n3"
pExample = parseString p' mempty s
tokenDigitExample = parseString (token (some digit)) mempty s
someTokenDigitExample = parseString (some (token (some digit))) mempty s

tokenWhole :: Parser Char
tokenWhole = token $ char 'a' >> char 'b'

tokenWholeExample = parseString tokenWhole mempty "a b"

tokenWholeExample2 = parseString tokenWhole mempty "ab ab"

tokenWholeExample3 = parseString (some tokenWhole) mempty "ab ab"

tokenWholeWithA = token (char 'a') >> char 'b'

tokenWholeExampleWithA = parseString tokenWholeWithA mempty "a b"

tokenWholeExampleWithA2 = parseString (some tokenWholeWithA) mempty "a b a b"

tokenBothAAndB :: Parser Char
tokenBothAAndB = token (char 'a') >> token (char 'b')

tokenBothAAndBExample = parseString (some tokenBothAAndB) mempty "a b a b"
