module MoreList where

consExample = 'c' : "hris"

headExample = head "Paul"

tailExample = tail "Paul"

takeExample = take 1 "Paul"
takeExample2 = take 0 "Paul"

dropExample = drop 2 "Paul"

infixOperatorExample = "Paul" !! 2

-- These are all considered unsafe because the throw exceptions on edge cases
exceptionalCase = "Paul" !! 6