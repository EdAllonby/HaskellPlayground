module LogParser where

import           Control.Applicative
import           Control.Monad

import           Text.Parser.LookAhead
import           Text.Trifecta

type Hour = Integer

type Minute = Integer

type Message = String

type Year = Integer

type Month = Integer

type Day = Integer

data Time = Time Hour Minute
    deriving Show

data LogEntry = Comment Message | TimeLogEntry Time Message (Maybe LogEntry)
    deriving Show

data Date = Date Year Month Day
    deriving Show

data LogEntryGroup = LogEntryGroup Date [LogEntry]
    deriving Show

newtype Log = Log [LogEntryGroup]
    deriving Show

parseTime :: Parser Time
parseTime = liftA2 Time integer (char ':' *> integer)

parseComment :: Parser LogEntry
parseComment = Comment <$> (string "--" >> spaces >> manyTill anyChar (void newline <|> eof))

parseTimeLogEntry :: Parser LogEntry
parseTimeLogEntry = do
    time <- parseTime <* spaces
    entry <- manyTill anyChar (try (lookAhead $ string "--") <|> (void newline >> return "") <|> (eof >> return ""))
    comment <- (Just <$> try parseComment) <|> return Nothing
    return $ TimeLogEntry time entry comment

parseLogEntry :: Parser LogEntry
parseLogEntry = try parseComment <|> try parseTimeLogEntry

parseLogEntryGroup :: Parser LogEntryGroup
parseLogEntryGroup = do
    date <- liftA3 Date (char '#' >> spaces >> integer) (char '-' >> integer) (char '-' >> integer)
    a <- many parseLogEntry
    return $ LogEntryGroup date a

parseLog :: Parser Log
parseLog = Log <$> many parseLogEntryGroup

main :: IO ()
main = do
    result <- parseFromFile parseLog "src/Log.txt"
    forM_ result print