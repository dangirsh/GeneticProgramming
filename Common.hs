module Common (
    randInt,
    randFloat,
    randElem,
    random,
    average,
    intAverage,
    every,
    g, n, m, selection_p, k, crossover_p, mutation_p
) where


import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (genericLength)


--returns a random integer between 0 and i-1, inclusive
randInt :: Int -> IO Int
randInt i = randomRIO (0, i - 1)


-- returns a random float between 0 and f
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


average :: Fractional a => [a] -> a
average l = (sum l) / (genericLength l)


intAverage :: Integral a => Fractional b => [a] -> b
intAverage = average . map fromIntegral


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


-- number of generations
g = 100 :: Int

-- size of population
n = 20 :: Int

-- size of solution
m = 10 :: Int

-- fraction to select
selection_p = 0.50 :: Float

-- number to select
k = round $ (fromIntegral n) * selection_p :: Int

-- probability of crossover
crossover_p = 0.5 :: Float

-- probability of mutation
mutation_p = 0.2 :: Float