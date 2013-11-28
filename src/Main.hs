module Main where


import Common (g, n, intAverage, every, plot)
import Solution (Solution, fitness, randomSol)
import Population (Population, nextPop, sortPop, initPop, diversity)
import System.Random (getStdGen, setStdGen)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans (lift)


topSol :: Population -> Solution
topSol = head . sortPop


-- TODO: maybe make a Stats type + lenses
getStats :: Population -> [Double]
getStats pop = map (($ pop)) [(fromIntegral . fitness . topSol)
                             ,(intAverage . map fitness)
                             ,(intAverage . map length)
                             ,diversity
                             ]


evolve :: Int -> Population -> WriterT [[Double]] IO Population
evolve 0 pop = return pop
evolve i pop = do
            tell $ [fromIntegral (g - i + 1) : getStats pop]
            (lift $ nextPop pop) >>= evolve (i - 1)


writeStats :: ([[Double]], Int) -> IO ()
writeStats = writeFile "out.txt" . unlines . map show . uncurry every


-- TODO: use Stats lens
plotFitness stats =
    plot "Fitness" "x" "y" "../plots/out.png" [1.0..(fromIntegral g)] $ map (!!1) stats


--main :: IO ()
main = do
    gen <- getStdGen
    setStdGen gen
    (pop, stats) <- initPop >>= (runWriterT . evolve g)
    writeStats (stats, 10)
    print $ topSol pop
    plotFitness stats