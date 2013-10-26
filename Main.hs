module Main where

import Data.List (sort)
import System.Random (setStdGen, getStdGen, randomRIO)
import Control.Applicative


--------Util------------

randInt n = randomRIO (0, n - 1)

randFloat f = randomRIO (0.0, f)

random = randFloat 1.0


-- GA params
g = 100 :: Int -- number of generations
n = 100 :: Int -- size of population
m = 5 :: Int -- size of solution
selection_p = 0.2 :: Float   -- fraction to select
k = round $ (fromIntegral m) * selection_p :: Int  -- number to select
crossover_p = 0.9 :: Float  -- probability of crossover
mutation_p = 0.1 :: Float   -- probability of mutation


--------Allele---------

type Allele = Int


mutationSize = 0.1 :: Float


mutateAllele :: Allele -> IO Allele
mutateAllele a = do
    p <- random
    --x <- randFloat mutationSize
    --let z = x - (mutationSize / 2)
    z <- randomRIO (-1, 1)
    return $ if p < mutation_p then a + z else a


--------Solution---------

type Fitness = Int

type Sol = [Allele]


fitness :: Sol -> Fitness
fitness sol = let (a, _) = foldl countInOrder (0, -1) sol in a
    where
        countInOrder (count, last) next
            | next > last = (count + 1, next)
            | otherwise = (count, next)


concatAlleles :: [Allele] -> [Allele] -> Sol
concatAlleles = (++)


splitSol :: Int -> Sol -> ([Allele], [Allele])
splitSol = splitAt


crossover :: Sol -> Sol -> IO Sol
crossover p1 p2 = do
    i <- randInt m
    let (a, _) = splitSol i p1
    let (_, b) = splitSol i p2
    return $ concatAlleles a b


mutateSol :: Sol -> IO Sol
mutateSol sol = do
    i <- randInt m
    let a = sol !! i
    ma <- mutateAllele a
    return $ (take (i - 1) sol) ++ [ma] ++ (drop i sol)


mate :: (Sol, Sol) -> IO Sol
mate (p1, p2) = do
    child <- crossover p1 p2
    mutateSol child


combine :: [Sol] -> [Sol] -> [Sol]
combine parents children = take k parents ++ take (m - k) children


randomSol :: IO Sol
randomSol = sequence $ replicate m (randInt 100)


-------------Population----------------

initPop :: IO [Sol]
initPop = sequence $ replicate n $ randomSol


sortPop :: [Sol] -> [Sol]
sortPop pop = snd . unzip . reverse . sort $ zip (map fitness pop) pop


select :: [Sol] -> IO [Sol]
select pop = return $ ((take k) . sortPop) pop


randomPair :: [Sol] -> IO (Sol, Sol)
randomPair parents = do
    i1 <- randInt k
    i2 <- randInt k
    return (parents !! i1, parents !! i2)


nextPop :: [Sol] -> IO [Sol]
nextPop pop = do
    parents <- select pop
    mates <- sequence $ replicate (n - k) (randomPair parents)
    children <- mapM mate mates
    return $ combine parents children


-------------GA--------------

runGA :: Int -> [Sol] -> IO [Sol]
runGA 0 pop = return pop
runGA i pop = do
    next <- nextPop pop
    runGA (i - 1) next


results :: IO Sol
results = do
    firstPop <- initPop
    lastPop <- runGA g firstPop
    return $ (head . sortPop) lastPop


main = do
    gen <- getStdGen
    setStdGen gen
    results >>= print