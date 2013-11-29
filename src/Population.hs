{-# LANGUAGE GADTs #-}


module Population (
    initPop,
    sortPop,
    nextPop,
    getDiversity
) where


import Common (n, k, randElem)
import Solution (Solution, randomSol, cmpSol, mate, hashSol)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Data.Set (fromList, size)


initPop :: Solution a => IO [a]
initPop = replicateM n randomSol


sortPop :: Solution a => [a] -> [a]
sortPop = sortBy $ flip cmpSol


-- truncation selection
select :: Solution a => [a] -> IO [a]
select pop = return $ take k . sortPop $ pop


combine :: Solution a => [a] -> [a] -> [a]
combine parents children = parents ++ children


randomPair :: [a] -> IO (a, a)
randomPair parents = do
    p1 <- randElem parents
    p2 <- randElem parents
    return (p1, p2)


nextPop :: Solution a => [a] -> IO [a]
nextPop pop = do
    parents <- select pop
    children <- replicateM (n - k) (randomPair parents >>= mate)
    return $ combine parents children


-- fraction of unique solutions in pop
getDiversity :: Solution a => [a] -> Double
getDiversity pop = fromIntegral (size $ fromList $ map hashSol pop) / fromIntegral (length pop)
