{-# LANGUAGE MultiParamTypeClasses #-}


module Population where


import GP
import Common (randomElem)
import Solution (Solution, randomSol, cmpSol, mate)


import Data.List (nub)
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import Control.Monad.ListM (sortByM)

type Population a = [a]


initPop :: Solution r t a => GP a (Population (r (t a)))
initPop = do
    n <- getParam populationSize
    replicateM n randomSol


sortPop :: Solution r t a => Population (r (t a)) -> GP a (Population (r (t a)))
sortPop = sortByM (flip cmpSol)


-- truncation selection
select :: Solution r t a => Int -> Population (r (t a)) -> GP a (Population (r (t a)))
select k pop = take k <$> sortPop pop


combine :: Population a -> Population a -> Population a
combine parents children = parents ++ children


randomPair :: Population a -> IO (a, a)
randomPair parents = do
    p1 <- randomElem parents
    p2 <- randomElem parents
    return (p1, p2)


nextPop :: Solution r t a => Population (r (t a)) -> GP a (Population (r (t a)))
nextPop pop = do
    let n = length pop
    p <- getParam selectionP
    let k = round $ p * fromIntegral n
    parents <- select k pop
    children <- replicateM (n - k) (liftIO (randomPair parents) >>= mate)
    return $ combine parents children


-- fraction of unique solutions in pop
-- TODO: use solution distance instead!
getDiversity :: Solution r t a => Population (r (t a)) -> Double
getDiversity pop = fromIntegral (length (nub pop)) / fromIntegral (length pop)
