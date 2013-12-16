{-# LANGUAGE MultiParamTypeClasses #-}


module Population where


import GP
import Common (randomElem)
import Solution (Solution, randomSol, cmpSol, mate)


import Data.List (sortBy, nub)
import Control.Monad (replicateM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)


type Population a = [a]


initPop :: Solution r t => GP (Population (r t))
initPop = do
    n <- asks populationSize
    replicateM n randomSol


sortPop :: Solution r t => Population (r t) -> Population (r t)
sortPop = sortBy $ flip cmpSol


-- truncation selection
select :: Solution r t => Int -> Population (r t) -> Population (r t)
select k = take k . sortPop


combine :: Population a -> Population a -> Population a
combine parents children = parents ++ children


randomPair :: Population a -> IO (a, a)
randomPair parents = do
    p1 <- randomElem parents
    p2 <- randomElem parents
    return (p1, p2)


nextPop :: Solution r t => Population (r t) -> GP (Population (r t))
nextPop pop = do
    let n = length pop
    p <- asks selectionP
    let k = round $ p * fromIntegral n
    parents <- lift . return $ select k pop
    children <- replicateM (n - k) (lift (randomPair parents) >>= mate)
    lift . return $ combine parents children


-- fraction of unique solutions in pop
getDiversity :: Solution r t => Population (r t) -> Double
getDiversity pop = fromIntegral (length (nub pop)) / fromIntegral (length pop)
