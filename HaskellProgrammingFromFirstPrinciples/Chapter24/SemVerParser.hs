module SemVerParser where

import           Control.Applicative
import           Text.Trifecta


data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Ord, Show)

parseVer :: Parser (Major, Minor, Patch)
parseVer = do
    major <- integer
    _     <- separatorParser
    minor <- integer
    _     <- separatorParser
    patch <- integer
    return (major, minor, patch)
    where separatorParser = char '.'

numOrStringWithPossibleDot :: Parser NumberOrString
numOrStringWithPossibleDot = do
    numberOrString <- NOSS <$> some letter <|> NOSI <$> decimal
    _              <- skipMany (oneOf ".")
    return numberOrString

parseSemVer :: Parser SemVer
parseSemVer = do
    (major, minor, patch) <- parseVer
    _                     <- option ' ' (char '-')
    r                     <- option [] (some numOrStringWithPossibleDot)
    _                     <- option ' ' (char '+')
    m                     <- option [] (some numOrStringWithPossibleDot)
    return $ SemVer major minor patch r m

big :: SemVer
big = SemVer 2 1 1 [] []

little :: SemVer
little = SemVer 2 1 0 [] []

parseSemVerExample :: IO ()
parseSemVerExample = do
    print $ parseString parseSemVer mempty "2.1.1"
    print $ parseString parseSemVer mempty "1.0.0-x.7.z.92"
    print $ parseString parseSemVer mempty "1.0.0+hello.there"
    print $parseString parseSemVer mempty "1.0.0-x.7.z.1111+hello.1.2.3"
    print $ big > little
