import Data.Char

-- Return wraps a pure value into an IO Action
-- Here 'test' will be bound to "hello", and sent to the terminal
-- But this is a bit redundant, you should use 'let' for this.
main = do
    test <- return "hello"
    let test' = "hello"
    putStrLn $ test ++ " " ++ test'