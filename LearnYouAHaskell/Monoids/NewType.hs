module NewType where

import           Control.Applicative

-- ZipList is a [] newtype which provides a different applicative implementation to []
zipListExample :: [Integer]
zipListExample = getZipList
  $ ZipList [(+ 1), (* 100), (* 5)] <*> ZipList [1, 2, 3]

-- A few ways we could define ZipList. Newtype is made exactly for this.
data ZipList' a = ZipList' [a]

data ZipList'' a = ZipList'' { getZipList'' :: [a] }

newtype ZipList''' a = ZipList''' { getZipList''' :: [a] }

-- newtypes can only have one data constructor
-- the following cannot be made a newtype
data Profession = Fighter
                | Archer
                | Accountant

data Race = Human
          | Elf
          | Orc
          | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

-- Can also use deriving
newtype CharList = CharList { getCharList :: String }
  deriving (Eq, Show)

charListExample :: CharList
charListExample = CharList "This is interesting"

charListEquatable :: Bool
charListEquatable = CharList "Hi" == CharList "Hi"

charListEquatable2 :: Bool
charListEquatable2 = CharList "Hi" == CharList "Bye"

-- Tuple functor which affects the first element
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair b) where
  fmap f (Pair (x, y)) = Pair (f x, y)

pairFunctorExample :: (Integer, Integer)
pairFunctorExample = getPair $ (+ 2) <$> Pair (1, 2)

-- Won't fail because of undefined - lazy
lazinessExample :: Integer
lazinessExample = head [1, 2, 3, undefined]

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- CoolBool is not a newtype, so it is not lazy evaluated.
helloMeFail :: String
helloMeFail = helloMe undefined

newtype CoolBoolLazy = CoolBoolLazy { getCoolBoolLazy :: Bool }

helloMe' :: CoolBoolLazy -> String
helloMe' (CoolBoolLazy _) = "hello"

helloMeSucceed :: String
helloMeSucceed = helloMe' undefined

type IntList = [Int] -- this is a type alias, can be interchanged

aliasExample :: [Int]
aliasExample = ([1, 2, 3] :: IntList) ++ ([1, 2, 3] :: [Int])
-- If you just want your type signature to look cleaner and be more descriptive, you probably want type synonyms.
-- If you want to take an existing type and wrap it in a new type in order to make it an instance of a type class, chances are you're looking for a newtype
-- If you want to make something completely new, odds are good that you;re looking for the data keyword.