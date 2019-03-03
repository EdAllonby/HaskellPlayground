module LearnParsers where

import           Text.Trifecta
import           Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

-- equivalent to char '1' >> stop
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) :: Parser Char -> Parser b -> Parser 
-- Any effect the Parser Char action had upon the monadic context remains.
one' :: Parser b
one' = one >> stop

one'' :: Parser ()
one'' = one >> eof

two :: Parser Char
two = char '2'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

zero :: Parser Char
zero = char '0'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

oneTwo'' :: Parser ()
oneTwo'' = one >> two >> eof

oneTwoThree :: Parser ()
oneTwoThree = one >> two >> char '3' >> eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "123"

testParseEOFSuccess :: IO ()
testParseEOFSuccess = testParse' oneTwoThree

p123 :: String -> Result String
p123 s = parseString (string s) mempty s

pNL :: [Char] -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    testParse $ choice [one, two, stop]
