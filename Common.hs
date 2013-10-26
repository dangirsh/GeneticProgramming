module Common (
    randInt,
    randFloat,
    randElem,
    random,
    g, n, m, selection_p, k, crossover_p, mutation_p
) where


import System.Random (randomRIO)


randInt i = randomRIO (0, i - 1)

randFloat f = randomRIO (0.0, f)

randElem l = do
    i <- randInt $ length l
    return $ l !! i

random = randFloat 1.0 :: IO Float


-- number of generations
g = 100 :: Int

-- size of population
n = 100 :: Int

-- size of solution
m = 5 :: Int

-- fraction to select
selection_p = 0.2 :: Float

-- number to select
k = round $ (fromIntegral m) * selection_p :: Int

-- probability of crossover
crossover_p = 0.9 :: Float

-- probability of mutation
mutation_p = 0.1 :: Float