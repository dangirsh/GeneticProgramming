module Population (
    Population,
    initPop,
    sortPop,
    nextPop,
    diversity
) where


import Common (n, k, randElem)
import Solution (Solution, randomSol, cmpSol, mate, hashSol)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Data.Set (fromList, size)


type Population = [Solution]


initPop :: IO Population
initPop = replicateM n randomSol


sortPop :: Population -> Population
sortPop = sortBy $ flip cmpSol


-- truncation selection
select :: Population -> IO Population
select pop = return $ take k . sortPop $ pop


combine :: [Solution] -> [Solution] -> Population
combine parents children = parents ++ children


randomPair :: [Solution] -> IO (Solution, Solution)
randomPair parents = do
    p1 <- randElem parents
    p2 <- randElem parents
    return (p1, p2)


nextPop :: Population -> IO Population
nextPop pop = do
    parents <- select pop
    children <- replicateM (n - k) (randomPair parents >>= mate)
    return $ combine parents children


-- fraction of unique solutions in pop
diversity :: Population -> Double
diversity pop = fromIntegral (size $ fromList $ map hashSol pop) / fromIntegral (length pop)
