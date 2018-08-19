-- 1) The type of MakeExample is Example (:t MakeExample)
data Example = MakeExample deriving Show

-- 2) Output from :info Example. You can determine that the instance is Show.:
--    data Example = MakeExample      -- Defined at forExample.hs:2:1
--    instance [safe] Show Example -- Defined at forExample.hs:2:37

-- 3) Output from :Info Example2 We can see that MakeExample2 expects a String.
--    data Example2 = MakeExample2 String -- Defined at forExample.hs:9:1
--    instance [safe] Show Example2 -- Defined at forExample.hs:9:47
data Example2  = MakeExample2 String deriving Show
