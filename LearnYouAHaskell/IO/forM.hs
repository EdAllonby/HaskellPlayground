import Control.Monad

main = do
    colours <- forM [1, 2, 3, 4] (\a -> do
        putStrLn $ "Which colour do you associate with the number " ++ show a ++ "?"
        colour <- getLine
        return colour)
    putStrLn "The colours that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colours

    
main2 = do
    colours <- mapM (\a -> do
        putStrLn $ "Which colour do you associate with the number " ++ show a ++ "?"
        colour <- getLine
        return colour)  [1, 2, 3, 4]
    putStrLn "The colours that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colours