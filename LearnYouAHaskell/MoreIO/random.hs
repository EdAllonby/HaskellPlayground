import System.Random

randomFloat :: (Float, StdGen)
randomFloat = random $ mkStdGen 100

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

infiniteRandom = take 5 $ randoms (mkStdGen 11) :: [Int]

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen)

finiteRandomsExample :: [Int]
finiteRandomsExample = fst . finiteRandoms 100 $ mkStdGen 100

randomRange :: (Float, StdGen)
randomRange = randomR (1,6) (mkStdGen 35565)

randomRanges :: [Char]
randomRanges = take 10 $ randomRs ('a', 'z') (mkStdGen 3)