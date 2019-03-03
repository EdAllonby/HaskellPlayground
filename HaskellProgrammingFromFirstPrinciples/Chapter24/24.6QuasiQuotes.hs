{-# LANGUAGE  QuasiQuotes #-}

module QuasiQuotes where

import           Control.Applicative
import           Text.RawString.QQ
import           Text.Trifecta

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

type NumberOrString = Either Integer String

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

parseNumberOrStringToken :: Parser NumberOrString
parseNumberOrStringToken = skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter) 
 
mainTokenise :: Result [NumberOrString]
mainTokenise = parseString (some (token parseNumberOrStringToken)) mempty eitherOr

main :: IO ()
main = do 
    let p f = parseString f mempty
    print $ p (some parseNumberOrString) eitherOr

data MyName = MyName String deriving Show