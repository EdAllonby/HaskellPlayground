module ChapterExercises where

-- 1
concata = concat [[1,2,3],[4,5,6]]

concatb = (++) [1,2,3] [4,5,6]

concatc = (++) "Hello" " world!"

-- Original ["hello" ++ "world]
concatd = concat ["Hello" ++ " World"]

infixe = "hello" !! 4

infixf = (!!) "hello" 4

takeg = take 4 "lovely"

takeh = take 3 "awesome"

-- 2
a2 = concat [[1 * 6], [2 * 6], [3 * 6]]
b2 = "rain" ++ drop 2 "elbow"
c2 = 10 * head [1, 2, 3]
d2 = (take 3 "Julie") ++ (tail "yes")
e2 = concat [tail [1, 2, 3],
             tail [4, 5, 6],
             tail [7, 8, 9]]

buildera x = x ++ "!"

builderb x = take 1 $ drop 4 x

-- 3
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4
letterIndex :: Int -> Char
letterIndex x = "Curry is Awesome" !! x

letterIndex2 :: Int -> String -> Char
letterIndex2 x xs = xs !! x

-- 5
rvrs x = concat [awesome, " ", is, " ", curry]
    where curry = take 5 x
          is = take 2 . drop 6 $ x
          awesome = take 7 . drop 9 $ x