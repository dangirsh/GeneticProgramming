
module Main where


import Common (g, n, intAverage, every, plot)
import Solution (Solution, fitness)
import Solution.ConcatSolution (ConcatSolution)
import Population (nextPop, sortPop, initPop, getDiversity)
import System.Random (getStdGen, setStdGen)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))


--topSol :: Solution a => Population -> a
topSol :: [ConcatSolution] -> ConcatSolution
topSol = head . sortPop


data PopStats =
    PopStats {
         psGeneration::Int
        ,psFitness::Double
        ,psAvFitness::Double
        ,psAvSolLength::Double
        ,psDiversity::Double
    } deriving Show


type GenStats = [PopStats]


getPopStats :: [ConcatSolution] -> Int -> PopStats
getPopStats pop i =
    PopStats {
         psGeneration = i
        ,psFitness = fromIntegral . fitness . topSol $ pop
        ,psAvFitness = intAverage . map fitness $ pop
        ,psAvSolLength =intAverage . map length $ pop
        ,psDiversity = getDiversity pop
    }


evolve :: Int -> [ConcatSolution] -> WriterT GenStats IO [ConcatSolution]
evolve 0 pop = return pop
evolve i pop = do
            tell [getPopStats pop (g - i + 1)]
            lift (nextPop pop) >>= evolve (i - 1)


writeStats :: (GenStats, Int) -> IO ()
writeStats = writeFile "out.txt" . unlines . map show . uncurry every


plotFitness :: GenStats -> IO ()
plotFitness genStats = plot title xlabel ylabel ("../plots/" ++ fname) xs ys
    where
        (title, xlabel, ylabel, fname) = ("Fitness", "x", "y", "fitness.png")
        xs = map psGeneration genStats
        ys = map psFitness genStats


main :: IO ()
main = do
    _ <- setStdGen <$> getStdGen
    (pop, genStats) <- initPop >>= (runWriterT . evolve g)
    writeStats (genStats, 10)
    print $ topSol pop
    plotFitness genStats
