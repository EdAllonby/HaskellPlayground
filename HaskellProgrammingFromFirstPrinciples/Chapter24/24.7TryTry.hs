module TryTry where

import           Control.Applicative
import           Text.Trifecta
import           Data.Ratio                     ( (%) )

type DecimalOrFraction = Either Integer Rational

parseFraction :: Parser Rational
parseFraction = do
    numerator   <- decimal
    _           <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = decimal

-- try will suppress errors
parseFractionOrDecimal :: Parser (Either Rational Integer)
parseFractionOrDecimal =
    (Left <$> try parseFraction) <|> (Right <$> parseDecimal)

main :: IO ()
main = do
    let p f = parseString f mempty
    print $ p (some parseFractionOrDecimal) "123"
