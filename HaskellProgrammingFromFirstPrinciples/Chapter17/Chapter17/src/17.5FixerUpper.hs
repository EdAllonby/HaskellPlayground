module FixerUpper where

    -- first = const <$> Just "Hello" <*> "World"
first = const <$> Just "Hello" <*> Just "World"

-- second = (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
second = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
second' = (,,,) <$> Just 90 <*> Just 10 <*> Nothing <*> Just [1, 2, 3]
