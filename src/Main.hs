{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import GP
--import Common
import Stats (RunStats, getPopStats)
import Plot (plotStats)
import Solution (Solution)
import Population (nextPop, initPop, sortPop)
--import Solution.TreeSolution (TreeSolution)
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
    --treeStatsList <- (batch batchSize :: IO [RunStats TreeSolution])
    --plotStats treeStatsList "../plots/tree"


params :: GPParams
params = GPParams {
    numGenerations = 10
   ,populationSize = 10
   ,solutionSize = 10
   ,selectionP = 0.3
}


main :: IO ()
main = setStdGen <$> getStdGen >> plotBatch 5 params
