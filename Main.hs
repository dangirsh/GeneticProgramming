module Main where


import Common
import Solution (Solution)
import Population
import System.Random (getStdGen, setStdGen)


runGA :: Int -> Population -> IO Population
runGA 0 pop = return pop
runGA i pop = nextPop pop >>= runGA (i - 1)


results :: IO Solution
results = initPop >>= runGA g >>= return . head . sortPop


main = do
    gen <- getStdGen
    setStdGen gen
    results >>= print