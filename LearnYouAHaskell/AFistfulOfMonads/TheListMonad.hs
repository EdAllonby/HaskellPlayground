module TheListMonad where

listMonad :: [Integer]
listMonad = concat (fmap (\x -> [x + 1]) [1 .. 5])

listMonad' :: [Integer]
listMonad' = [1 .. 5] >>= (\x -> [x + 1])

chaining :: [(Integer, Char)]
chaining = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

chainingDo :: [(Integer, Char)]
chainingDo = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

-- Both do notation and list comprehension translate to >>=.
chainingComprehension = [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]

