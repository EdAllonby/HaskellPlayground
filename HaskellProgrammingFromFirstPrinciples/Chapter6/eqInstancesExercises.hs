-- 1
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where 
    (==) (TisAn i)  (TisAn i') = i == i'

-- 2
data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = a == c && b == d

-- 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

-- 4
data Pair a = 
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- 5
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

-- 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a' 
    (==) _ _ = False