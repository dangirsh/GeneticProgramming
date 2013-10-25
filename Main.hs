module Main where

import Data.List (sort)
import System.Random (setStdGen, getStdGen, randomRIO)
import Control.Applicative


n = 10
m = 15
k = 5


-----------------

type Sol = Int


--crossover :: Sol -> Sol -> Sol
--crossover = (+)


--mutate :: [Sol] -> [Sol]
--mutate [] = []
--mutate (c:cs) = c + 1 : mutate cs


mate :: (Sol, Sol) -> IO Sol
mate (p1, p2) = return $ p1 + p2


combine :: [Sol] -> [Sol] -> [Sol]
combine parents children = take k parents ++ take (n - k) children

-----------------------------


initPop :: IO [Sol]
initPop = sequence $ replicate m $ randomRIO (0,17)


sortPop :: [Sol] -> [Sol]
sortPop = reverse . sort


select :: [Sol] -> IO [Sol]
select pop = return $ ((take k) . sortPop) pop


randomPair :: [Sol] -> IO (Sol, Sol)
randomPair parents = do
    i1 <- randomRIO (0, k - 1)
    i2 <- randomRIO (0, k - 1)
    return (parents !! i1, parents !! i2)


nextPop :: [Sol] -> IO [Sol]
nextPop pop = do
    parents <- select pop
    mates <- sequence $ replicate (m - k) (randomPair parents)
    children <- mapM mate mates
    return $ combine parents children

---------------------------


runGA :: Int -> [Sol] -> IO [Sol]
runGA 0 pop = return pop
runGA i pop = do
    next <- nextPop pop
    runGA (i - 1) next


results :: IO Sol
results = do
    firstPop <- initPop
    lastPop <- runGA n firstPop
    return $ (head . sortPop) lastPop


main = do
    gen <- getStdGen
    setStdGen gen
    results >>= print