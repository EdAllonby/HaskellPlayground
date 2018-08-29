import System.IO

-- This is how withFile could be implemented using bracket.
main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")