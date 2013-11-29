module Solution (
    Solution,
    Fitness,
    randomSol,
    fitness,
    mate,
    sizeSol,
    cmpSol,
    hashSol
) where


type Fitness = Int


class Solution sol where
    randomSol :: IO sol
    fitness :: sol -> Fitness
    mate :: (sol, sol) -> IO sol
    sizeSol :: sol -> Int
    cmpSol :: sol -> sol -> Ordering
    hashSol :: sol -> Int