module Solution (
    Solution,
    Fitness,
    randomSol,
    fitness,
    mate,
    cmpSol,
    hashSol
) where


type Fitness = Int


class Solution sol where
    randomSol :: IO sol
    fitness :: sol -> Fitness
    mate :: (sol, sol) -> IO sol
    cmpSol :: sol -> sol -> Ordering
    hashSol :: sol -> Int