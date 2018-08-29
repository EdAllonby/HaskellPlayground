import Control.Monad

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- Is the equivalent of the following.
mainWithoutSequence = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

-- when mapping prints, we need to sequence it to transform it to a single IO ()
sequencePrint = sequence $ map print [1..5]

-- This is a common pattern, so mapM exists for this sake.
mapMPrint = mapM print [1..5]

-- Without [(),(),(),(),()] returned
mapMPrint' = mapM_ print [1..5]