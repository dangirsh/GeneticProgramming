module Solution where

import Common

type Fitness = Int


class (Show s, Eq s, Compatible s) => Solution s where
    randomSol :: IO s
    fitness :: s -> Fitness
    mate :: (s, s) -> IO s
    sizeSol :: s -> Int
    cmpSol :: s -> s -> Ordering

