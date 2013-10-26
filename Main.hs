module Main where


import Common
import Solution (Solution)
import Population
import System.Random (getStdGen, setStdGen)


runGA :: Int -> Population -> IO Population
runGA 0 pop = return pop
runGA i pop = do
    next <- nextPop pop
    runGA (i - 1) next


results :: IO Solution
results = do
    firstPop <- initPop
    lastPop <- runGA g firstPop
    return $ (head . sortPop) lastPop


main = do
    gen <- getStdGen
    setStdGen gen
    r <- results
    print r