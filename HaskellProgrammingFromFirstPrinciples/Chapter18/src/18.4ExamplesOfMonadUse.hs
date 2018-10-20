module ExamplesOfMonadUse where

-- List

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: [ ] a -> (a -> [ ] b) -> [ ] b
-- or more syntactically common
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- Each element of xs binds to x
-- We create a new list
-- Because of bind, the new lists are 'joined' to produce a single final list.
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x * x, x * x] else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x then [x * x, x * x] else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = do
    x <- xs
    if even x then [x * x] else [x]

twiceWhenEvenReturn :: [Integer] -> [Integer]
twiceWhenEvenReturn xs = do
    x <- xs
    if even x then return $ x * x else return x


twiceAll :: [Integer] -> [Integer]
twiceAll xs = do
    x <- xs
    let twiceX = x * x
    return twiceX

-- Maybe

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

data Cow = Cow { name :: String, age :: Int, weight :: Int } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in  if n == "Bess" && w > 499 then Nothing else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = case noEmpty name' of
    Nothing   -> Nothing
    Just name -> case noNegative age' of
        Nothing  -> Nothing
        Just age -> case noNegative weight' of
            Nothing     -> Nothing
            Just weight -> weightCheck (Cow name age weight)

-- Do syntax isn't just for IO.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    name   <- noEmpty name'
    age    <- noNegative age'
    weight <- noNegative weight'
    weightCheck (Cow name age weight)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = noEmpty name' >>= \name ->
    noNegative age' >>= \age ->
        noNegative weight' >>= \weight -> weightCheck (Cow name age weight)


bindDroppingTest = Nothing >>= undefined
bindingDroppingTest2 = Just 1 >>= undefined


-- Either

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
