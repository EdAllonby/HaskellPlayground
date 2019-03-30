import           Control.Monad.State
import qualified Data.IntMap                   as M

digits :: Int -> [Int]
digits = fmap (subtract 48 . fromEnum) . show

step :: Int -> Int
step = sum . fmap (^ (2 :: Int)) . digits

terminatorOld :: Int -> Int
terminatorOld n | n == 1    = 1
                | n == 89   = 89
                | otherwise = terminatorOld n'
    where n' = step n

terminator' :: Int -> State (M.IntMap Int) Int
terminator' n = do
    m <- get
    if M.member n m
        then return (m M.! n)
        else do
            t <- terminator (step n)
            modify (M.insert n t)
            return t

terminator :: Int -> State (M.IntMap Int) Int
terminator n = if (n == 1) || (n == 89) then return n else terminator' n

terminators :: [Int] -> State (M.IntMap Int) [Int]
terminators = mapM terminator

main :: IO ()
main = print $ length $ filter (== 89) $ evalState
    (terminators [1 .. 10000000])
    M.empty
