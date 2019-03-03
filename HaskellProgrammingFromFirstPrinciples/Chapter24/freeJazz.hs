module FreeJazz where

import           Text.Trifecta

-- Parsing Chars
gimmeA :: Parser Char
gimmeA = char 'a'

parseA :: String -> Result Char
parseA = parseString gimmeA mempty

useParseA :: Result Char
useParseA = parseA "abc"

useExample :: Result Char
useExample = parseString (char 'a' >> char 'b' >> char 'c') mempty "abcd"

-- Parsing Strings
parseStringExample :: Result String
parseStringExample = parseString (string "abc") mempty "abc"

parseStringFailure :: Result String
parseStringFailure = parseString (string "abc") mempty "ab"

stop :: Parser a
stop = unexpected "stop pls"

parseStopExample :: Result a
parseStopExample = parseString (char 'a' >> stop) mempty "abcdef"

