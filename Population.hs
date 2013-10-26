module Population (
    Population,
    initPop,
    sortPop,
    nextPop
) where


import Common
import Solution
import Data.List (sort)


type Population = [Solution]


initPop :: IO Population
initPop = sequence $ replicate n $ randomSol


sortPop :: Population -> Population
sortPop pop = snd . unzip . reverse . sort $ zip (map fitness pop) pop


select :: Population -> IO Population
select pop = return $ ((take k) . sortPop) pop


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
    mates <- sequence $ replicate (n - k) (randomPair parents)
    children <- mapM mate mates
    return $ combine parents children