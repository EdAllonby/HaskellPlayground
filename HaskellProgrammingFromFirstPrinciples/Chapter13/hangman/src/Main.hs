module Main where

import Data.Bool (bool)
import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
                in l >= minWordLength
                && l < maxWordLength

countWords :: WordList -> IO Int
countWords (WordList wl) = return (length wl)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed guesses) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed ++ ". Total guesses: " ++ show guesses

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle p _ _ _) c = c `elem` p

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed  (Puzzle _ _ g _) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word filledInSoFar s guesses) c = Puzzle word newFilledInSoFar (c : s) newGuessCount
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar
          newGuessCount = bool (guesses + 1) guesses (charInWord p c)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do putStrLn "You already guessed that character, pick something else!"
                        return puzzle
        (True, _) -> do putStrLn "This character was in the word, filling in the word accordingly"
                        return (fillInCharacter puzzle guess)
        (False, _) -> do putStrLn "This character wasn't in the word, try again"
                         return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guesses) =
    when (guesses > 7) $
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
    when (all isJust filledInSoFar) $
        do putStrLn "You win!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    case guess of
            [c] -> handleGuess puzzle c >>= runGame
            _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle