module MoreExamples where

import           Text.Trifecta

doesNotStartWithA :: Result Char
doesNotStartWithA = parseString (notChar 'a') mempty "baba"

startWithA :: Result Char
startWithA = parseString (notChar 'a') mempty "abba"

anyCharExample :: Result Char
anyCharExample = parseString anyChar mempty "123"
