-- This is a list comprehension example. What we do here is create 3 lists (a, b and c) which are cross-joined to give all their combinations as a list of tuples.
-- We then have 2 predicates defined to filter the cross-joined tuples. The first is to verify if the tuple represents a right-angled triangle.
-- The second predicate is then used to determine if the sum of all sides matches the input.
-- Questions I have to answer:
--  Why does Haskell think 'rightTriangles' has a type of: (Num c, Eq c, Enum c) => c -> c -> [(c, c, c)]? I've defined a 'simpler' type below which works. What does this mean?

-- Extracting the core logic here which takes the limit, type of triangle and filter predicate.
triangleFilter hypotenuseLimit triangleTypeFilter filter = [(a,b,c) | c <- [1..hypotenuseLimit], b <- [1..c], a <- [1..b], triangleTypeFilter a b c, filter a b c]

-- We create a partially applied right-angle generator here to define any type of right-angled triangles.
rightAngledTriangleGenerator hypotenuseLimit = triangleFilter hypotenuseLimit (\a b c -> a^2 + b^2 == c^2) 

rightTriangles :: Int -> Int -> [(Int, Int, Int)]
rightTriangles hypotenuseLimit summationTarget = rightAngledTriangleGenerator hypotenuseLimit (\a b c -> a + b + c == summationTarget)