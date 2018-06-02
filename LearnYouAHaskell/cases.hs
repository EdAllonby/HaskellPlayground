-- Cases allow pattern matching anywhere in your code. Similar to switch statements in c languages
headOld :: [a] -> a
headOld [] = error "no head for empty lists"
headOld (x:_) = x

headWithCase :: [a] -> a
headWithCase xs = case xs of [] -> error "no head for empty lists"
                             (x:_) -> x

-- Case in middle of expression example
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

-- Another way using where. What function is defined with 3 cases.
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty"
          what [x] = "an empty list"
          what xs = "a longer list"