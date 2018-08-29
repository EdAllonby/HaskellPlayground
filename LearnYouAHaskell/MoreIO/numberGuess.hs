import System.Random
import Control.Monad (when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randomNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
            then putStrLn "You are correct"
            else putStrLn $ "Sorry, it was " ++ show randomNumber
        askForNumber newGen

-- When calling newStdGen, we create a new generator when used with getStdGen
anotherWayUsingNewStdGen = do
    gen <- getStdGen
    let (randomNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randomNumber
        newStdGen
        main