-- Sectioning lets you choose which side to partially apply an infix operator.
-- You can either partially apply before or after.
x = 5
twoToPowerOf = (2^)
powerOfTwo = (^2)

twoToPowerOfFive = twoToPowerOf x
fiveToPowerOfTwo = powerOfTwo x

-- This works for other types of infix operator too.
sectionedConcat = (++ " woot!")
exampleSectionedConcat = sectionedConcat "first"

-- You can also use it when the function is normally a prefix form.
isElementOfOneToTen = (`elem` [1..10])
isNineElementOf = (9 `elem`)

isTwoElementOfOneToTen = isElementOfOneToTen 1
isNineElementOfTwoToFive = isNineElementOf [2..5]