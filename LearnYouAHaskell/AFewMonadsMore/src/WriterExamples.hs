{-# LANGUAGE InstanceSigs #-}

module WriterExamples where

isBigGang :: Int -> Bool
isBigGang = (> 9)

isBigGangLogged :: Int -> (Bool, String)
isBigGangLogged x = (isBigGang x, "Compared gang size to 9")

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (a, l) ab = let (b, l2) = ab a in (b, l <> l2)

useApplyLog :: (Integer, [String])
useApplyLog = (1, pure "Started with 1.") `applyLog` (\x -> (x + 1, pure "added one."))

newtype MyWriter w a = Writer { runMyWriter :: (a, w) }

instance Functor (MyWriter w) where
    fmap :: (a -> b) -> MyWriter w a -> MyWriter w b
    fmap ab (Writer (a, w)) = Writer (ab a, w)

instance (Monoid m) => Applicative (MyWriter m) where
    pure :: (Monoid m) => a -> MyWriter m a
    pure a = Writer (a, mempty)

    (<*>) :: Monoid m => MyWriter m (a -> b) -> MyWriter m a -> MyWriter m b
    (<*>) (Writer (ab, w)) (Writer (a, w2)) = Writer (ab a, w <> w2)

instance (Monoid m) => Monad (MyWriter m) where
    return :: (Monoid m) => a -> MyWriter m a
    return = pure

    (>>=) :: (Monoid m) => MyWriter m a -> (a -> MyWriter m b) -> MyWriter m b
    (>>=) (Writer (a, w)) aWmb = let (Writer (b, w2)) = aWmb a in Writer (b, w <> w2)