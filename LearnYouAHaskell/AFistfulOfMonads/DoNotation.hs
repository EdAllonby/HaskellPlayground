module DoNotation where

withoutDo :: Maybe String
withoutDo = Just 3 >>= (\x -> Just (show x ++ "!"))

withoutDoNested :: Maybe String
withoutDoNested = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

similarTo :: String
similarTo = let x = 3
                y = "!"
            in show x ++ y

doNotation :: Maybe String
doNotation = do
  x <- Just 3 -- x == 3
  y <- Just "!" -- y == "!"
  Just $ show x ++ y

doNotationGeneralised :: (Monad m) => m String
doNotationGeneralised = do
  let x = 3 -- x == 3
  let y = "!" -- y == "!"
  return $ show x ++ y