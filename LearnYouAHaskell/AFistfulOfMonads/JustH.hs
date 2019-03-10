module JustH where

justH :: Maybe Char
justH = do
  (x:_) <- Just "hello"
  return x

-- The Maybe implementation for Fail is Nothing.
-- Here the pattern match fails, so this returns nothing.
failExample :: Maybe Char
failExample = do
  (x:_) <- Just ""
  return x