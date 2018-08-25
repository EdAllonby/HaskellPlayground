import Data.Bool

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str =  make (vowelOffset str) str
    where vowelOffset = foldr (\x acc -> bool (acc - 1) (acc + 1) (x `elem` vowels)) 0
          make i str 
            | i > 0 = Nothing
            | otherwise = Just (Word' str)