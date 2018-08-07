data Mood = Blah | Woot deriving Show

-- 1) Type constructor is Mood
-- 2) If a function requires type constructor Mood, you could use data constructor Blah or data constructor Woot
-- 3) changeMood :: Mood -> Woot won't work because you're returning a Woot, where you should return Mood

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah