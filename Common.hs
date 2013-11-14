module Common (
    randInt,
    randFloat,
    randElem,
    random,
    g, n, m, selection_p, k, crossover_p, mutation_p
) where


import System.Random (randomRIO)


randInt :: Int -> IO Int
randInt i = randomRIO (0, i - 1)


randFloat :: Float -> IO Float
randFloat f = randomRIO (0.0, f)


randElem :: [a] -> IO a
randElem l = do
    i <- randInt $ length l
    return $ l !! i


random :: IO Float
random = randFloat 1.0 :: IO Float


-- number of generations
g = 200 :: Int

-- size of population
n = 200 :: Int

-- size of solution
m = 20 :: Int

-- fraction to select
selection_p = 0.3 :: Float

-- number to select
k = round $ (fromIntegral n) * selection_p :: Int

-- probability of crossover
crossover_p = 0.9 :: Float

-- probability of mutation
mutation_p = 0.1 :: Float