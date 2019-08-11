{-# LANGUAGE InstanceSigs #-}

module MyParser where

import           Control.Monad

{- Using an F# guide (Scott W, F# for fun and profit) and giving it a go in Haskell.
Lots of nice parallels between the two languages, so not too difficult to port.
And I feel it's a good exercise to improve knowledge on parser combinators. 
https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/ -}
aParser :: String -> (Bool, String)
aParser []           = (False, "")
aParser str@(a : as)
    | a == 'A' = (True, as)
    | otherwise = (False, str)

aParserExample :: (Bool, String)
aParserExample = aParser "ABC"

aParserExampleFail :: (Bool, String)
aParserExampleFail = aParser "ZAC"

pCharBefore :: Char -> String -> Either String (Char, String)
pCharBefore _ [] = Left "No more input"
pCharBefore charToMatch (a : as)
    | a == charToMatch = Right (a, as)
    | otherwise = Left $ "Expecing " <> [ charToMatch ] <> ". Got " <> [ a ]

pCharBeforeExample :: Either String (Char, String)
pCharBeforeExample = pCharBefore 'd' "dab"

-- Wrap the `String -> Either String (Char, String)` in a newtype, and make it polymorphic.
newtype MyParser a =
    MyParser { runParser :: String -> Either String (a, String) }

pChar :: Char -> MyParser Char
pChar charToMatch = MyParser inner
  where
    inner []       = Left "No more input"
    inner (a : as)
        | a == charToMatch = Right (a, as)
        | otherwise =
            Left $ "Expecing " <> [ charToMatch ] <> ". Got " <> [ a ]

pCharExample :: Either String (Char, String)
pCharExample = runParser (pChar 'x') "xyz"

andThen :: MyParser a -> MyParser b -> MyParser (a, b)
andThen (MyParser a) (MyParser b) = MyParser inner
  where
    inner input = case a input of
        (Left err) -> Left err
        (Right (value1, remaining1)) -> case b remaining1 of
            (Left err) -> Left err
            (Right (value2, remaining2)) ->
                Right ((value1, value2), remaining2)

-- Is this applicative sequence (i.e. '>>' operator)?
(.>>.) :: MyParser a -> MyParser b -> MyParser (a, b)
(.>>.) = andThen

aThenB :: MyParser (Char, Char)
aThenB = pChar 'a' .>>. pChar 'B'

andThenExample :: Either String ((Char, Char), String)
andThenExample = runParser aThenB "aBcDeF"

orElse :: MyParser a -> MyParser a -> MyParser a
orElse parserA parserB = MyParser inner
  where
    inner input = case runParser parserA input of
        (Right res) -> Right res
        (Left _)    -> case runParser parserB input of
            (Left err) -> Left err
            (Right (value, remaining)) -> Right (value, remaining)

-- Is this Alternative operator?
(<|>) :: MyParser a -> MyParser a -> MyParser a
(<|>) = orElse

aOrB :: MyParser Char
aOrB = pChar 'a' <|> pChar 'B'

orElseExample :: Either String (Char, String)
orElseExample = runParser aOrB "aBcDeF"

aThenBorC :: MyParser (Char, Char)
aThenBorC = pChar 'a' .>>. (pChar 'B' <|> pChar 'C')

andThenOrExample :: Either String ((Char, Char), String)
andThenOrExample = runParser aThenBorC "aCD"

choice :: [MyParser a] -> MyParser a
choice = foldr1 (<|>)

anyOf :: String -> MyParser Char
anyOf = choice . fmap pChar

parseDigit :: MyParser Char
parseDigit = anyOf [ '0' .. '9' ]

parseDigitExample :: Either String (Char, String)
parseDigitExample = runParser parseDigit "123"

instance Functor MyParser where
    fmap :: (a -> b) -> MyParser a -> MyParser b
    fmap f (MyParser p) = MyParser $ \input ->
        (\(val, remaining) -> (f val, remaining)) <$> p input

instance Applicative MyParser where
    pure :: a -> MyParser a
    pure a = MyParser $ \x -> Right (a, x)

    (<*>) :: MyParser (a -> b) -> MyParser a -> MyParser b
    (<*>) (MyParser ab) a = MyParser $ \inp -> case ab inp of
        (Left err)       -> Left err
        (Right (g, out)) -> runParser (g <$> a) out

instance Monad MyParser where
    return :: a -> MyParser a
    return = pure

    (>>=) :: MyParser a -> (a -> MyParser b) -> MyParser b
    (>>=) (MyParser a) aMyParserB = MyParser $ \inp -> case a inp of
        (Left err)       -> Left err
        (Right (g, out)) -> runParser (aMyParserB g) out

parseThreeDigits :: MyParser ((Char, Char), Char)
parseThreeDigits = parseDigit .>>. parseDigit .>>. parseDigit

parseThreeDigitsMapToString :: MyParser String
parseThreeDigitsMapToString = (\((a, b), c) -> [ a ] <> [ b ] <> [ c ])
    <$> parseThreeDigits

parseThreeDigitsToInt :: MyParser Int
parseThreeDigitsToInt = read <$> parseThreeDigitsMapToString