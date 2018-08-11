negateSum = negate . sum
negateSumApplied = negateSum [1..5]

f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

fPointFree :: Int -> [Int] -> Int
fPointFree = foldr (+)

lengthFilterA = length . filter (== 'a')

blah x = x
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)

blahPF = id
addAndDropPF = const . (1 +)
reverseMkTuplePF = flip (,)
reverseTuplePF = uncurry (flip (,))