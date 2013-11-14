module Common (
    randInt,
    randFloat,
    randElem,
    random,
    average,
    g, n, m, selection_p, k, crossover_p, mutation_p
) where


import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (genericLength)


--returns a random integer between 0 and i-1, inclusive
randInt :: Int -> IO Int
randInt i = randomRIO (0, i - 1)


-- returns a random float between 0 and 1
randFloat :: Float -> IO Float
randFloat f = randomRIO (0.0, f)


-- returns a random element of l
randElem :: [a] -> IO a
randElem l = do
    i <- randInt $ length l
    return $ l !! i


-- returns n random elements from l, possibly with duplicates
randElems :: Int -> [a] -> IO [a]
randElems 0 _ = return []
randElems n l = replicateM n (randElem l)


random :: IO Float
random = randFloat 1.0 :: IO Float


average :: Integral a => [a] -> a
average l = div (sum l) (genericLength l)


-- number of generations
g = 100 :: Int

-- size of population
n = 100 :: Int

-- size of solution
m = 50 :: Int

-- fraction to select
selection_p = 0.3 :: Float

-- number to select
k = round $ (fromIntegral n) * selection_p :: Int

-- probability of crossover
crossover_p = 0.9 :: Float

-- probability of mutation
mutation_p = 0.1 :: Float