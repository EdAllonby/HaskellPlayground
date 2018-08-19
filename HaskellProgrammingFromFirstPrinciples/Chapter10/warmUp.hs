stops = "pbtdkg"
vowels = "aeiou"

make3TuplesA a b = [(s1, v, s2) | s1 <- a, s2 <- a, v <- b]

make3TuplesAStartingWithP a b = [(s1, v, s2) | s1 <- a, s2 <- a, v <- b, s1 == 'p']

nouns = ["woman", "boy", "dog", "neighbour"]
verbs = ["run", "hit", "travel", "become"]

make3TuplesB a b = [(n1, v, n2) | n1 <- a, n2 <- a, v <- b ]

averageWordLength x = div (sum (map length (words x))) (length (words x))

averageWordLengthPrecise x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

averageWordLengthPrecise' x = (fromIntegral . sum . map length . words $ x) / (fromIntegral . length . words $ x)