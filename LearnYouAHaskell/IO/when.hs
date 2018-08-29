import Control.Monad

main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

-- Is the equivalent of the following.
-- 'when' packages this logic up nicely.
mainWithoutWhen = do
    input <- getLine
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()