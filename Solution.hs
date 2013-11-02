module Solution (
    Solution,
    Fitness,
    fitness,
    mate,
    randomSol
) where


import Common
import Allele


type Solution = [Allele]


type Fitness = Int


fitness :: Solution -> Fitness
fitness solution = let (a, _) = foldl countInOrder (0, -1) solution in a
    where
        countInOrder (count, last) next
            | next > last = (count + 1, next)
            | otherwise = (count, next)


concatAlleles :: [Allele] -> [Allele] -> Solution
concatAlleles = (++)


split :: Int -> Solution -> ([Allele], [Allele])
split = splitAt


crossover :: Solution -> Solution -> IO Solution
crossover p1 p2 = do
    i <- randInt m
    let (a, _) = split i p1
    let (_, b) = split i p2
    return $ concatAlleles a b


mutate :: Solution -> IO Solution
mutate solution = do
    i <- randInt m
    ma <- mutateAllele (solution !! i)
    return $ (take (i - 1) solution) ++ [ma] ++ (drop i solution)


mate :: (Solution, Solution) -> IO Solution
mate (p1, p2) = crossover p1 p2 >>= mutate


randomSol :: IO Solution
randomSol = sequence $ replicate m (randInt 100)