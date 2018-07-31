import qualified Data.Map as Map

data WithoutRecordPerson = WithoutRecordPerson String String Int Float String String deriving (Show)

guy = WithoutRecordPerson "Buddy" "Hole" 43 184.1 "123-4412" "Peanut"

-- Cumbersome method
firstNameOfWithoutRecordPerson (WithoutRecordPerson firstname _ _ _ _ _) = firstname
firstNameOfGuy = firstNameOfWithoutRecordPerson guy

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavour :: String } deriving (Show)

-- With record syntax, you can construct in any order.
createPersonDifferentOrder = Person {lastName = "Robert", firstName = "Steve", flavour="Vanilla", age=44, height=1.80, phoneNumber="123-1234"}

-- And you can get the firstname much easier than the first method above
firstNameOfPerson (Person {firstName=f}) = f
firstNameOfGuy2 = firstNameOfPerson createPersonDifferentOrder

maybeCharA = Just 'a'

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company=c, model=m, year=y}) =
    "This" ++ c ++ " " ++ m ++ " war made in " ++ show y


-- Not useful. This is too generic.
data ParameterisedCar a b c = ParameterisedCar { pcompany :: a
                                  , pmodel :: b
                                  , pyear :: c
                                  } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

vectorExample = Vector 2 9 3 `vmult` (Vector 3 1 2 `dotProd` Vector 1 1 2)

data Person2 = Person2 { firstName2 :: String
                       , lastName2 :: String
                       , age2 :: Int
                       } deriving (Eq, Show, Read)

mikeD = Person2 {firstName2 = "Michael", lastName2="Steves", age2=32}

-- Because both have a class constraint to Eq a, we can do this
isEqualToSomethingElse = mikeD `elem` [mikeD]

-- Type values are nullary (don't have any fields) so they can be part of enum.
-- They have bounds (min and max), and we can add all the other derived types too.
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

afterMonday = succ Monday
beforeWednesday = pred Wednesday
weekdays = [Monday .. Friday]
allDays = [minBound .. maxBound] :: [Day]

type DaysSynonym = [Day]
allDaysUsingSynonym = [minBound .. maxBound] :: DaysSynonym

phoneBook :: [(String, String)]
phoneBook = 
    [("betty", "565-1231")
    ,("bob", "123-9811")
    ,("mike", "918-1999")]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

-- Without synonyms, it would look like this:
-- inPhoneBook :: String -> String -> [(String, String)] -> Bool
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- type synonyms can also be parameterized. We can then do something like this: 
-- (Eq k) => k -> AssocList k v -> Maybe v
type AssocList k v = [(k, v)]

-- The following are equivalent, because we can partially apply the second parameter - just like function arguments
type IntMap v = Map.Map Int v
type IntMap2 = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
        [(100,(Taken, "ZD39I"))
        ,(101,(Free, "12345"))
        ,(102,(Free, "54321"))]