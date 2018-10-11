-- https://www.youtube.com/watch?v=t1e8gqXLbsU
-- Hutton's Computerphile episode
-- This is an interesting Youtube video on explaining a Monad.
-- As I'm watching this, I'll be trying some of the things Hutton explains.

-- First Hutton introduces a data type for dividing expressions
data Expr = Val Int | Div Expr Expr deriving (Show)

divider :: Expr -> Int
divider (Val a) = a
divider (Div x y) = div (divider x) (divider y)

divideVal = divider $ Val 1

dividerExpr = Div (Val 6) (Val 2)
divideMe2 = divider dividerExpr

-- Hutton then gives an exercise to try nested expressions
divideNestedSomething = Div (Val 6) (Div (Val 3) (Val 1))
divideMe3 = divider divideNested

-- Now we make it safe to divide by 0
safeDiv :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0 then Nothing else Just $ div n m

safeDivider :: Expr -> Maybe Int
safeDivider (Val a) = Just a
safeDivider (Div x y) = case safeDivider x of
                            Nothing -> Nothing
                            Just x -> case safeDivider y of
                                Nothing -> Nothing
                                Just y -> safeDiv x y

divideNested = Div (Val 6) (Div (Val 0) (Val 0))
divideMe3' = safeDivider divideNested

-- Then we introduce the Maybe Monad to simplify the safe divider
monadicSafeDivider :: Expr -> Maybe Int
monadicSafeDivider (Val n) = return n
monadicSafeDivider (Div x y) = monadicSafeDivider x >>= (\n -> monadicSafeDivider y >>= (\o -> safeDiv n o))

divideMeMonadic = monadicSafeDivider divideNestedSomething

-- And finally we refactor the above with Do notation to make it easier to read (or more sequencial how you view it) with less lamda expressions
monadicDoSafeDivider :: Expr -> Maybe Int
monadicDoSafeDivider (Val n) = return n
monadicDoSafeDivider (Div x y) = do
    a <- monadicDoSafeDivider x
    b <- monadicDoSafeDivider y
    safeDiv a b

divideMeDoMonadic = monadicDoSafeDivider divideNestedSomething

-- To summarise, we've rediscovered the Maybe type.
-- This is return and the sequencing operator (>>=)
-- return is a bridge from pure to impure - i.e. Int -> Maybe Int
-- Sequence can sequence things. Maybe a -> (a -> Maybe b) -> Maybe b