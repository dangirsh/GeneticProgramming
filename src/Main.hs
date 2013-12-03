module Main where


import Common (g, n, intAverage, every, plot)
import Solution (Solution, fitness, sizeSol)
import Solution.TreeSolution (TreeSolution)
import Population (Population, nextPop, sortPop, initPop)
import System.Random (getStdGen, setStdGen)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))


topSol :: Solution a => Population a -> a
topSol = head . sortPop


data PopStats =
    PopStats {
         psGeneration::Int
        ,psFitness::Double
        ,psAvFitness::Double
        ,psAvSolSize::Double
        --,psDiversity::Double
    } deriving Show


type GenStats = [PopStats]


getPopStats :: Solution a => Population a -> Int -> PopStats
getPopStats pop i =
    PopStats {
         psGeneration = i
        ,psFitness = fromIntegral . fitness . topSol $ pop
        ,psAvFitness = intAverage . map fitness $ pop
        ,psAvSolSize =intAverage . map sizeSol $ pop
        --,psDiversity = getDiversity pop
    }


evolve :: Solution a => Int -> Population a -> WriterT GenStats IO (Population a)
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
        xs = map ((*n) . psGeneration) genStats
        ys = map psFitness genStats


runGP :: Solution a => Population a -> IO ()
runGP startPop = do
    (endPop, genStats) <- (runWriterT . evolve g) startPop
    writeStats (genStats, 10)
    print $ topSol endPop
    plotFitness genStats


main :: IO ()
main = do
    _ <- setStdGen <$> getStdGen
    startPop <- (initPop :: IO (Population TreeSolution))
    runGP startPop