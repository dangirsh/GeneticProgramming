module Common where


import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (genericLength)
import Foreign.Marshal.Utils (fromBool)
import Control.Applicative
import Control.Monad.Trans.Reader (ReaderT)


--returns a random integer between 0 and i-1, inclusive
randomInt :: Int -> IO Int
randomInt i | i > 0 = randomRIO (0, i - 1)
            | otherwise = error "Integer must be positive."


-- returns a random Double between 0 and f
randDouble :: Double -> IO Double
randDouble f = randomRIO (0.0, f)


-- returns a random element of l
randomElem :: [a] -> IO a
randomElem l =
    case l of
        [] -> error "No elements."
        _  -> do
                i <- randomInt $ length l
                return $ l !! i


-- returns i random elements from l, possibly with duplicates
randomElems :: Int -> [a] -> IO [a]
-- randomElems 0 _ = return []
randomElems i l = replicateM i (randomElem l)


random :: IO Double
random = randDouble 1.0 :: IO Double


average :: Fractional a => [a] -> a
average l = sum l / genericLength l


intAverage :: Integral a => Fractional b => [a] -> b
intAverage = average . map fromIntegral


every :: [a] -> Int -> [a]
every xs i = case drop (i-1) xs of
              (y:ys) -> y : every ys i
              [] -> []


data GPParams = GPParams {
    numGenerations :: Int
   ,populationSize :: Int
   ,solutionSize :: Int
}


type GP = ReaderT GPParams IO


-- number of generations
g :: Int
g = 10

-- size of population
n :: Int
n = 10

-- size of solution
m :: Int
m = 10

-- fraction to select
selection_p :: Double
selection_p = 0.50

-- number to select
k :: Int
k = round $ (fromIntegral n) * selection_p

-- probability of crossover
crossover_p :: Double
crossover_p = 0.5

-- probability of mutation
mutation_p :: Double
mutation_p = 0.2 :: Double


type ValueType = Bool


type InputType = [ValueType]


trueSolution :: InputType -> ValueType
trueSolution = even . sum . map fromBool


numInputs :: Int
numInputs = 6


inputs :: [InputType]
inputs = replicateM numInputs [True, False]