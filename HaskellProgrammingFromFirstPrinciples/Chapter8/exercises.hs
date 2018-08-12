-- 1
-- [[True, False], [True, True], [False, True]]
-- type of [[Bool]]

-- 2
-- Same type
-- [[True, False], [True, True], [False, True]]
-- [[3 == 3], [6 > 5], [3 < 4]]

-- 3
func :: [a] -> [a] -> [a]
func x y = x ++ y
-- x and y must be the same type
-- x and y must both be lists
-- if x is a String then y must be a String

valid1 = func "Hello World" -- Partial
valid2 = func "Hello" "World"
valid3 = func ["Hello", "World"] -- Partial, expects a list of strings for second arg

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "whoops"

frappe :: String -> String
frappe = flippy "haha"

woohoo :: String
woohoo = appedCatty "woohoo"

frappe1 :: String
frappe1 = frappe "1"

appedCattyApplied :: String
appedCattyApplied = appedCatty (frappe "blue")

cattyConnyApplied :: String
cattyConnyApplied = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))

cattyConnyApplied2 = cattyConny (flippy "Pugs" "are") "awesome"

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 0 = 0
recursiveSum n = n + recursiveSum (n - 1)

recursiveMultiply :: (Integral a) => a -> a -> a
recursiveMultiply n 1 = n
recursiveMultiply n count = n + recursiveMultiply n (count - 1)

