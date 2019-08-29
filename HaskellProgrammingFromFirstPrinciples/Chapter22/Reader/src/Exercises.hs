{-# LANGUAGE InstanceSigs #-}

module Exercises where

import           Control.Applicative

import           Data.Maybe

import           Prelude             hiding (uncurry)

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = liftA2 (,) (z' n) (z' n)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry abc (a, b) = abc a b

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(> 3), (< 8), even] 7
    print $ and . sequA $ 4
    print $ fmap sequA s'
    print $ fmap bolt ys

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap ab (Reader ra) = Reader $ ab . ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader ra) aRrb = Reader $ \r -> (runReader $ aRrb $ ra r) r

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person = Person { humanName :: HumanName, dogName :: DogName, address :: Address }
    deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName, dogsAddress :: Address }
    deriving (Eq, Show)

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 abc fa fb = abc <$> fa <*> fb

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
    name <- asks dogName
    addy <- asks address
    return $ Dog name addy

getDogRM'' :: Reader Person Dog
getDogRM'' = liftA2 Dog (asks dogName) (asks address)

introduceDog :: Reader Person String
introduceDog = do
    dog <- getDogRM''
    return $ "This is Mr. " <> (getDogsName (dogsName dog))
  where
    getDogsName :: DogName -> String
    getDogsName (DogName name) = name

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap ab (State sas) = State $ \t -> let (a, s) = sas t in (ab a, s)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State sab) (State sa) = State $ \s -> let (ab, s1) = sab s in let (a, s2) = sa s1 in (ab a, s2)

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State sa) aSsb = State $ \s ->
        let (a, s1) = sa s in let (State state) = aSsb a in let newState = state s1 in newState

get :: State a a
get = State $ \s -> (s, s)

put :: b -> State b ()
put newState = State $ const ((), newState)

evalState :: State s a -> s -> a
evalState (State st) s = let (a, _) = st s in a

execState :: State s a -> s -> s
execState (State st) s = let (_, s1) = st s in s1

stackyStack :: State [Int] ()
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3] then put [8, 3, 1] else put [9, 2, 1]

inc :: State Integer ()
inc = do
    curr <- get
    put $ curr + 1

dec :: State Integer ()
dec = do
    curr <- get
    put $ curr - 1

data GameState = GameState { name :: String, age :: Int }
    deriving Show

-- test :: State Integer ()
test :: State Integer Integer
test = do
    inc
    inc
    inc
    get

-- incrementAge :: State GameState Int
incrementAge :: State GameState ()
incrementAge = do
    state <- get
    let stateAge = age state
    put $ GameState { name = name state, age = stateAge + 1 }

test2 = do
    incrementAge
    incrementAge