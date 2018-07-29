-- Example: import qualified Data.List as L (nub, sort)
-- numUniques = length . L.nub

import Data.List
import Data.Char
import qualified Data.Map as Map -- clashes with prelude and list
import Control.Monad

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

allWords = words "these are some words in a sentence"
allWords2 = words "these     are   some words  in a    sentence"

allWordsGrouped = group . words $ "these are are some duplicate words words words"
allWordsGroupedSorted = group . sort . words $ "there are some are some duplicate words there in in here some"
allGroupedWordCount = map (\ws -> (head ws, length ws)) . group . sort . words

-- Also known as isInfixOf
isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)

encode offset = map (chr . (+ offset) . ord)
decode shift = encode (negate shift)

test = foldl' (+) 0 (replicate 100000000 1)

sumOfDigits :: Int -> Int
sumOfDigits = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find ((== n) . sumOfDigits) [1..]

firstTo40 :: Maybe Int
firstTo40 = firstTo 40

phoneBook :: Map.Map String String
phoneBook = Map.fromList
    [("betty", "555-2938")
    ,("bonnie", "452-2910")
    ,("patsy", "123-4121")
    ,("lucille","989-1921")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKeyMaybeRecursive :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyMaybeRecursive key [] = Nothing
findKeyMaybeRecursive key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKeyMaybeRecursive key xs

findKeyMaybeFold :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyMaybeFold key xs = foldr(\(k, v) acc -> if key == k then Just v else acc) Nothing xs

findBetty = Map.lookup "betty" phoneBook

newPhoneBook = Map.insert "grace" "123-4567" phoneBook

sizeDifferenceOfPhoneBooks =  (Map.size newPhoneBook) - (Map.size phoneBook)

string2digits :: String -> [Int]
-- Same as. but point-free: string2digits s = map digitToInt . filter isDigit $ s
string2digits = map digitToInt . filter isDigit

digitisedPhoneBook = Map.map string2digits phoneBook

phoneBookWithManyNumbers =
    [("betty", "555-2938")
    ,("bonnie", "452-2910")
    ,("bonnie", "471-4910")
    ,("patsy", "123-4121")
    ,("patsy", "453-4161")
    ,("patsy", "723-4111")
    ,("lucille","989-1921")
    ]

phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap2 xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
patsyNumbers = Map.lookup "patsy" $ phoneBookToMap2 phoneBookWithManyNumbers
patsyNumberCount = liftM length patsyNumbers

fromListWithMax = Map.fromListWith max [(2,100), (3, 1), (2,101), (3, -1)]