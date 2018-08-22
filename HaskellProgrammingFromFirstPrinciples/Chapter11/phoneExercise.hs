import Data.Char
import Data.Bool
import Data.List
import Data.Maybe

type Digit = Char

type Presses = Int

newtype DaPhone = DaPhone [(Digit, String)]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

phone = DaPhone [ ('1', "")
                , ('2', "abc")
                , ('3', "def")
                , ('4', "ghi")
                , ('5', "jkl")
                , ('6', "mno")
                , ('7', "pqrs")
                , ('8', "tuv")
                , ('9', "wxyz")
                , ('*', "^")
                , ('0', " +_")
                , ('#', ".,")]

getButtons :: DaPhone -> Digit -> [(Digit, String)]
getButtons ph@(DaPhone xs) c = go xs c []
  where go (btn:btns) char result
          | isUpper char = go xs (toLower char) result ++ getButtons ph '*'
          | char `elem` snd btn = result ++ [btn]
          | fst btn == char = result ++ [btn]
          | otherwise = go btns char result

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c = reverse . go  $ getButtons phone c
    where go [] = []
          go (x:xs)
            | fst x == '*' = ('*', 1) : go xs
            | otherwise = (fst x, 1 + fromJust (elemIndex (toLower c) (snd x))) : go xs

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldr (\x acc -> reverseTaps phone x ++ acc) []

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_,p) acc -> acc + p) 0

matchingLetterCountInString :: Char -> String -> Int
matchingLetterCountInString c = foldr (\x acc -> bool acc (acc + 1) (x == c)) 0

mostPopularLetter :: String -> Char
mostPopularLetter str@(c:_) = foldr (\x acc -> bool acc x (matchingLetterCountInString x str > matchingLetterCountInString acc str)) c . filter (/= ' ') $ str

mostPopularLetterCost :: DaPhone -> String -> Presses
mostPopularLetterCost phone = letterCost phone . mostPopularLetter

letterCost :: DaPhone -> Char -> Presses
letterCost phone c = fingerTaps (reverseTaps phone c)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord xs = foldr (\x acc -> bool acc x (isNextWordMorePopular x acc)) "" ws
                    where
                        ws = concatMap words xs
                        isNextWordMorePopular a b = wordOccurrences a ws > wordOccurrences b ws
                        wordOccurrences w ws = length (filter (==w) ws)
