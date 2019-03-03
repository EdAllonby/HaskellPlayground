module UnitOfSuccess where

import           Text.Trifecta

parseFirstInt :: Result Integer
parseFirstInt = parseString integer mempty "123abc456"

parseOnlyInt :: Result ()
parseOnlyInt = parseString (integer >> eof) mempty "123abc" -- only int and EOF are allowed

integerParser :: Parser Integer
integerParser = do
    val <- decimal
    _   <- eof
    return val

goodNumber :: Result Integer
goodNumber = parseString integerParser mempty "123"

badNumber :: Result Integer
badNumber = parseString integerParser mempty "123whatami"
