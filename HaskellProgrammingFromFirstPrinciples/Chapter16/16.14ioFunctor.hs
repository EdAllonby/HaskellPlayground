getInt :: IO Int
getInt = fmap read getLine

andMeToo = fmap (++ " and me too!") getLine

-- Is the same as this. But using fmap is cleaner
meTooIsm :: IO String
meTooIsm = do
    input <- getLine
    return (input ++ "and me too!")