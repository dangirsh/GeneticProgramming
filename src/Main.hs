{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import GP
import Stats (RunStats, getPopStats)
import Plot (plotStats)
import Solution (Solution)
import Population (nextPop, initPop, sortPop)
import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)

import System.Random (getStdGen, setStdGen)
import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)


evolve :: Solution r t => GP (RunStats (r t))
evolve = do
    g <- asks numGenerations
    (best, stats) <- runWriterT $ foldl f (lift initPop) [1..g]
    lift . print . head . sortPop $ best
    return stats
    where
        f acc _ = do
            pop <- acc
            tell [getPopStats pop]
            lift (nextPop pop)


batch :: Solution r t => Int -> GPParams -> IO [RunStats (r t)]
batch batchSize p =
    forM [1..batchSize] (\i -> print i >> runGP evolve p)


plotBatch :: Int -> GPParams -> IO ()
plotBatch batchSize p = do
    concatStatsList <- (batch batchSize p :: IO [RunStats ConcatSolution])
    plotStats concatStatsList "../plots/concat"
    treeStatsList <- (batch batchSize p :: IO [RunStats TreeSolution])
    plotStats treeStatsList "../plots/tree"


params :: GPParams
params = GPParams {
    numGenerations = 50
   ,populationSize = 50
   ,solutionSize = 50
   ,selectionP = 0.3
   ,crossoverP = 0.9
   ,mutationP = 0.1
}


main :: IO ()
main = setStdGen <$> getStdGen >> plotBatch 3 params
