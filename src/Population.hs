{-# LANGUAGE MultiParamTypeClasses #-}


module Population where


import Common (n, k, randomElem)
import Solution (Solution, randomSol, cmpSol, mate)
import Data.List (sortBy, nub)
import Control.Monad (replicateM)


type Population a = [a]


initPop :: Solution r t => IO (Population (r t))
initPop = replicateM n randomSol


sortPop :: Solution r t => Population (r t) -> Population (r t)
sortPop = sortBy $ flip cmpSol


-- truncation selection
select :: Solution r t => Population (r t) -> IO (Population (r t))
select pop = return $ take k . sortPop $ pop


combine :: Population a -> Population a -> Population a
combine parents children = parents ++ children


randomPair :: Population a -> IO (a, a)
randomPair parents = do
    p1 <- randomElem parents
    p2 <- randomElem parents
    return (p1, p2)


nextPop :: Solution r t => Population (r t) -> IO (Population (r t))
nextPop pop = do
    parents <- select pop
    children <- replicateM (n - k) (randomPair parents >>= mate)
    return $ combine parents children


-- fraction of unique solutions in pop
getDiversity :: Solution r t => Population (r t) -> Double
getDiversity pop = fromIntegral (length (nub pop)) / fromIntegral (length pop)
