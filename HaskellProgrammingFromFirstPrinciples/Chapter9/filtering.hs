multiplesOfThree = filter (\x -> x `mod` 3 == 0) [1..30]

multiplesOfThreeLength = length . filter (\x -> x `mod` 3 == 0) $ [1..30]

myFilter xs = filter (\x -> x /= "a" && x /= "an" && x /= "the" ) $ words xs