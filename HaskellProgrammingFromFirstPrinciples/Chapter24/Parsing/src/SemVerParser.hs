module SemVerParser where

import           Control.Applicative

import           Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer
    deriving (Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Show)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = try (NOSI <$> integer <* notFollowedBy letter) <|> (NOSS <$> some alphaNum)

parseDotSeparatedNumberOrString :: Parser [NumberOrString]
parseDotSeparatedNumberOrString = parseNumberOrString `sepBy` symbol "."

parseRelease :: Parser Release
parseRelease = char '-' >> parseDotSeparatedNumberOrString

parseMetadata :: Parser Metadata
parseMetadata = char '+' >> parseDotSeparatedNumberOrString

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    r <- try parseRelease <|> return []
    m <- try parseMetadata <|> return []
    eof
    return $ SemVer major minor patch r m

majMinPatch :: String
majMinPatch = "2.1.1"

complex :: String
complex = "1.0.0-x.7.z.92"

-- runSemVerParser :: String -> Result SemVer
runSemVerParser :: IO ()
runSemVerParser = do
    let p = parseString parseSemVer mempty
    print $ p "2.1.1"
    print $ p "1.0.0-x.7.z.92"
    print $ p "1.0.0-gamma+002"
    print $ p "1.0.0-beta+oof.sha.41af286"