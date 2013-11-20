module Main where


import Common (g, n, intAverage, every)
import Solution (Solution, fitness, randomSol)
import Population (Population, nextPop, sortPop, initPop, diversity)
import System.Random (getStdGen, setStdGen)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans (lift)



topSol :: Population -> Solution
topSol = head . sortPop


getStats :: Population -> [Float]
getStats pop = map (($ pop)) [diversity
                             ,(fromIntegral . fitness . topSol)
                             ,(intAverage . map fitness)
                             ,(intAverage . map length)]


evolve :: Int -> Population -> WriterT [[Float]] IO Population
evolve 0 pop = return pop
evolve i pop = do
            tell $ [fromIntegral (g - i + 1) : getStats pop]
            (lift $ nextPop pop) >>= evolve (i - 1)


writeStats :: [[Float]] -> Int -> IO ()
writeStats stats n = writeFile "out.txt" $ unlines $ map show $ every n stats


main :: IO ()
main = do
    gen <- getStdGen
    setStdGen gen
    (pop, stats) <- initPop >>= (runWriterT . evolve g)
    writeStats stats 10
    print $ topSol pop