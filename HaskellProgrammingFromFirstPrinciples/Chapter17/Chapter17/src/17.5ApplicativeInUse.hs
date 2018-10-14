module ApplicativeInUse where

import           Control.Applicative
import           Data.Char

-- With the list Applicative, we are mapping a plurality of functions over a plurality of values

-- [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
listExample = [(+ 1), (* 2)] <*> [2, 4]

tupleExample = (,) <$> [1, 2] <*> [3, 4]
-- Similar to
tupleExample' = [(,) 1, (,) 2] <*> [3, 4]

-- The liftA2 function gives us another way to write this
tupleExample'' = liftA2 (,) [1, 2] [3, 4]

addExample = (+) <$> [1, 2] <*> [3, 5]
addExample' = liftA2 (+) [1, 2] [3, 5]

maxExample = max <$> [1, 2] <*> [1, 4]
maxExample' = liftA2 max [1, 2] [1, 4]

l = lookup 3 [(3, "hello")]
lLength = fmap length $ l

c (x : xs) = toUpper x : xs
upperL = fmap c $ l

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

concat = (++) <$> f 3 <*> g 7
concat' = liftA2 (++) (f 3) (g 7)

adding = (+) <$> h 5 <*> m 1
adding' = liftA2 (+) (h 5) (m 1)

addingNothing = (+) <$> h 5 <*> m 6
addingNothing' = liftA2 (+) (h 5) (m 6)

ioLifting = (++) <$> getLine <*> getLine
ioLiftingLength = length <$> ioLifting
