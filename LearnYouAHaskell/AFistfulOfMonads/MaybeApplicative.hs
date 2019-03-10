module MaybeApplicative where

applicativeExample :: Maybe Integer
applicativeExample = max <$> Just 3 <*> Just 4

addOneWrapped :: Num a => a -> Maybe a
addOneWrapped x = Just $ x + 1

addOneMonad :: Maybe Integer
addOneMonad = Just 2 >>= addOneWrapped

addOneMonad' :: Maybe Integer
addOneMonad' = do
  x <- Just 2
  y <- addOneWrapped x
  return y

-- A manual bind
applyMaybe :: Maybe t -> (t -> Maybe a) -> Maybe a
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

-- (>>=) substituted with applyMaybe
addOneMonadNew :: Maybe Integer
addOneMonadNew = Just 2 `applyMaybe` addOneWrapped

anotherMonadExample :: Maybe Integer
anotherMonadExample = Just 9 >>= \x -> return (x + 9)