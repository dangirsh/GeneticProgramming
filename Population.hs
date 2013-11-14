module Population (
    Population,
    initPop,
    sortPop,
    nextPop
) where


import Common (n, k, randElem)
import Solution (Solution, randomSol, cmpSol, mate)
import Data.List (sortBy)
import Control.Monad (replicateM)

type Population = [Solution]


initPop :: IO Population
initPop = replicateM n randomSol


sortPop :: Population -> Population
sortPop = reverse . sortBy cmpSol


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