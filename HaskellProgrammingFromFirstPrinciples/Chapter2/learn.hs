module Learn where

x = 10 * 5 + y

myResult = x * 5

y = 10

-- Exercises: Heal the Sick
area x = 3.14 * (x * x)
double x = x * 2
x1 = 7
y1 = 10
f = x1 + y1

-- (-) is syntactic sugar for negate. The following are identical
subtract1 = 2000 + (-1234)
subtract2 = 2000 + (negate 1234)

-- $ iks least precedent right operator
dollar1 = (2^) (2 + 2)
dollar2 = (2^) $ 2 + 2
dollarMore = (2^) $ (*30) $ 2 + 2

-- The following is known as sectioning
sectionPlusOne = (+1) 3

-- But you should use subtract for -. This is because the compiler doesn't know if you mean negate
sectionNegativeOne = (subtract 1) 2