import Data.Monoid

-- Lists form a monoid under concatenation.
joiningLists = mappend [1, 2, 3] [4, 5, 6]
joiningIdentity = mappend [1..5] []
joiningIdentity' = mappend [] [1..5]
joiningIdentity'' = mappend [1..5] mempty
joiningIdentity''' = mappend mempty [1..5]
concatenatingLists = mconcat [[1..3], [4..6]]

-- Integers form a monoid under summation and multiplication.
sumAppend = getSum $ mappend (Sum 1) (Sum 5)
productAppend = getProduct $ mappend (Product 5) (Product 5)

-- <> is infix version of mappend
sumAppend' = getSum $ Sum 1 <> Sum 9
productAppend' = getProduct $ Product 2 <> Product 5

sumAppendIdentity = getSum $ mempty <> Sum 10
productAppendIdentity = getProduct $ mempty <> Product 20

multipleAppend = getSum $ Sum 2 <> Sum 3 <> Sum 4
multipleAppend' = getSum $ mconcat [Sum 8, Sum 6, Sum 4]

foldedAppend = foldr mappend mempty ([2, 4, 6] :: [Product Int])

allMonoid = getAll $ All True <> All True <> All False
anyMonoid = getAny $ Any True <> Any True <> Any False

firstMonoid = getFirst $ First Nothing <> First (Just 1) <> First (Just 2) <> First Nothing <> First (Just 3)
lastMonoid = getLast $ Last Nothing <> Last (Just 1) <> Last (Just 2) <> Last Nothing <> Last (Just 3)