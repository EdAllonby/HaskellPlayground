{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (aToB, s' ) = f s
            (a   , s'') = g s'
        in  (aToB a, s'')

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi p) >>= k = Moi $ \s -> let (x, s') = p s in runMoi (k x) s'

get :: Moi s s
get = Moi $ \x -> (x, x)

getTest :: (String, String)
getTest = runMoi get "I am a person" -- should be ("I am a person", "I am a person")

put :: s -> Moi s ()
put s = Moi $ const ((), s)

putTest :: ((), String)
putTest = runMoi (put "to be put") "current" -- should be ((), "to be put")

exec :: Moi s a -> s -> s
exec (Moi sa) s = let (_, s') = sa s in s'

execTest1 :: String
execTest1 = exec (put "returned") "state" -- should be "returned"

execTest2 :: String
execTest2 = exec get "I am state" -- should be "I am state"

eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a, _) = sa s in a

evalTest :: String
evalTest = eval get "toast" -- should be "toast"

modify :: (s -> s) -> Moi s ()
modify ss = Moi $ \s -> ((), ss s)

modifyTest1 :: ((), Int)
modifyTest1 = runMoi (modify (+ 1)) 0 -- should be ((), 1)

modifyTest2 :: ((), Int)
modifyTest2 = runMoi (modify (+ 1) >> modify (+ 1)) 0 -- should be ((), 2)
