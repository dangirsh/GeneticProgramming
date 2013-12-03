module Population where


import Common (n, k, randomElem)
import Solution (Solution, randomSol, cmpSol, mate)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Data.Set (fromList, size)


type Population a = [a]


initPop :: Solution a => IO (Population a)
initPop = replicateM n randomSol


sortPop :: Solution a => Population a -> Population a
sortPop = sortBy $ flip cmpSol


-- truncation selection
select :: Solution a => Population a -> IO (Population a)
select pop = return $ take k . sortPop $ pop


combine :: Population a -> Population a -> Population a
combine parents children = parents ++ children


randomPair :: Population a -> IO (a, a)
randomPair parents = do
    p1 <- randomElem parents
    p2 <- randomElem parents
    return (p1, p2)


nextPop :: Solution a => Population a -> IO (Population a)
nextPop pop = do
    parents <- select pop
    children <- replicateM (n - k) (randomPair parents >>= mate)
    return $ combine parents children


-- fraction of unique solutions in pop
--getDiversity :: Population a -> Double
--getDiversity pop = fromIntegral (size $ fromList $ map hashSol pop) / fromIntegral (length pop)
