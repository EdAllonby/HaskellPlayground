-- Guards are used to check if some property of those passed values match a predicate (true/false)
-- They're similar to if/else statements but more elegant / readable

-- 'Otherwise' acts as a catch all.
bmiTellSimpleGuard :: Double -> String
bmiTellSimpleGuard bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Good weight"
    | bmi <= 30.0 = "Overweight"
    | otherwise = "Obese"

-- Calculation inside guard predicate
bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Underweight"
    | weight / height ^ 2 <= 25.0 = "Good weight"
    | weight / height ^ 2 <= 30.0 = "Overweight"
    | otherwise = "Obese"

-- Max implementation using guards
max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

-- You can use the infix style (`) when specifying functions, too.
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

-- Note that you need to line up all the variable names
-- Where bodies are locally scoped. They work here, but do not work across function bodies.
bmiTellUsingWhere :: Double -> Double -> String
bmiTellUsingWhere weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Good weight"
    | bmi <= overweight = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          overweight = 30.0

-- Take a look at 'inlineLet4' example to understand how '(skinny, normal, overweight) = (18.5, 25.0, 30.0)' works.
-- It's basically taking a 3-Tuple and deconstructing it so you can use each of the parts independently. (for example 'let (a, b, c) = (1,2,3) in a' will return '1'.)
bmiTellUsingWhereAndPatternMatch :: Double -> Double -> String
bmiTellUsingWhereAndPatternMatch weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Good weight"
    | bmi <= overweight = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / height ^ 2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)

-- 2 ways to implement this. First is using where clause pattern matching. Second way is using pattern matching in arguments list (less code / more readable)
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- You can declare functions in where blocks when using list comprehension
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- Let is similar to where, but defined at the top. They are locally scoped inside its binding
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

inlineLet = 4 * (let a = 9 in a + 1) + 2

inlineLet2 = [let square x = x * x in (square 5, square 3, square 2)]

inlineLet3 = (let a = 100; b = 200; c = 300 in a * b * c, let foo = "hey "; bar = "bro" in foo ++ bar)

-- Using some pattern matching
inlineLet4 = (let (a, b, c) = (1, 2, 3) in a) * 100

-- Let in list comprehension.
-- We don't need an 'in' here because it's directly used.
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- We can also add a predicate to filter certain bmi values
calcBmisFatFilter :: [(Double, Double)] -> [Double]
calcBmisFatFilter xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]