module Main where


import Common (g)
import Solution (Solution, fitness)
import Population (Population, nextPop, sortPop, initPop)
import System.Random (getStdGen, setStdGen)


evolve :: Population -> IO Population
evolve = f g
    where
        f 0 pop = return pop
        f i pop = nextPop pop >>= f (i - 1)


results :: IO Solution
results = initPop >>= evolve >>= return . head . sortPop


main :: IO ()
main = do
    gen <- getStdGen
    setStdGen gen
    r <- results
    print r
    print $ fitness r