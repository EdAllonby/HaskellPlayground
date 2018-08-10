data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Eq, Show)

-- Only Mood is accepted
-- If you try to input 9, it will fail because it doesn't accept Num
-- Blah > Woot won't work because it doesn't implement the Ord typeclass
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Ord, Show)
data Yeah = Yeah Bool deriving (Eq, Ord, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

phew = Papu (Rocks "chases") (Yeah True)
truth = Papu (Rocks "ahhh") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x
