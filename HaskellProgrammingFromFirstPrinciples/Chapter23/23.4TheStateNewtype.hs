module TheStateNewtype where

newtype State s a = State { runState :: s -> (a, s) }

-- very similar to Reader
newtype Reader r a = Reader { runReader :: r -> a }

st :: State String Int
st = State $ (,) (1 :: Int)

runningState :: (Int, String)
runningState = runState st "Hello World"

-- Isomorphic newtype
type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }
sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- Note that random looks an awful lot like State here:
-- random :: (Random a) => StdGen -> (a, StdGen)
-- State { runState ::     s      -> (a, s) }
