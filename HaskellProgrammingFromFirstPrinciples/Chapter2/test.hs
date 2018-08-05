sayHello :: String -> IO ()
sayHello x =
    putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

-- Exercises: Comprehension Check
-- Exercise 1
half x = x / 2
square x = x * x

-- Exercise 2
squareTimesPi x = 3.14 * (x * x)
squareTimesPi2 x = 3.14 * (square x)
squareTimesPi3 x = pi * (square x)

-- Infix as prefix by wrapping in parentheses
addPrefix = (+) 10 2

-- Some things are right associative, like power
powerLeft = (2 ^ 3 ) ^ 4
powerRight = 2 ^ 3 ^ 4

-- Exercises: Parentheses and Association
-- (8 + 7 * 9) is different to ((8 + 7) * 9) because multiplication has a higher precedence than addition
sumAndMult1 a b c = a + b * c
sumAndMult2 a b c = (a + b) * c 
-- ((x * 2) + (y * 2)) is the same as (x * 2 + y * 2)
perimeter1 x y = (x * 2) + (y * 2)
perimeter2 x y = x * 2 + y * 2
-- x / 2 + 9 is different to x / (2 + 9) because division has a higher precedence than addition
divAndSum1 x = x / 2 + 9
divAndSum2 x = x / (2 + 9)
