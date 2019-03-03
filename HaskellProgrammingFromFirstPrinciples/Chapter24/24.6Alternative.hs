module Alternative where

import           Text.Trifecta
import           Control.Applicative

someExample :: Result String
someExample = parseString (some letter) mempty "blah123"

integerExample :: Result Integer
integerExample = parseString integer mempty "1234"

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

main :: IO ()
main = do
    let p f = parseString f mempty
    print $ p (some letter) a
    print $ p integer b
    print $ p parseNos a
    print $ p parseNos b
    print $ p (many parseNos) c -- zero or more
    print $ p (some parseNos) c -- one or more, if zero then error
    print $ p (many letter) a

alternativeExample1 :: Maybe Integer
alternativeExample1 = Just 1 <|> Just 2

alternativeExample2 :: Maybe Integer
alternativeExample2 = Nothing <|> Just 2

alternativeExample3 :: Maybe Integer
alternativeExample3 = Just 1 <|> Nothing

alternativeExample4 :: Maybe a
alternativeExample4 = Nothing <|> Nothing
