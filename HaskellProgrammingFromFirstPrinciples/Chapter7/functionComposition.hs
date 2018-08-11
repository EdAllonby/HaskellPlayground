negateSum = negate . sum $ [1..5]

takeReverse = take 5 . reverse $ [1..10]

takeEnumFrom = take 5 . enumFrom $ 3

takeFilterEnumFrom = take 5 . filter odd . enumFrom $ 3