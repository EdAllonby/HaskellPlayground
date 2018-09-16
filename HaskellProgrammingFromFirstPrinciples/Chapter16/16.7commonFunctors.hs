replaceWithP = const 'p'

replace = replaceWithP 10000
replace' = replaceWithP (Just 10)

replaceJust = fmap replaceWithP (Just 10)
replaceList = fmap replaceWithP [1..5]
replaceString = fmap replaceWithP "hello" -- String is just a List of Char
replaceEmpty = fmap replaceWithP []
replaceNothing = fmap replaceWithP Nothing

-- Function Functors will be discussed later when looking at the 'Reader' chapter
functionFunctor = fmap (+1) negate

n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]

lmsReplace = fmap replaceWithP lms
lmsReplace' = (fmap . fmap) replaceWithP lms -- Twice lifted
lmsReplace'' = (fmap . fmap . fmap) replaceWithP lms -- Thrice lifted