{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import GP (runGP)
import Common
import Stats (RunStats)
import Plot (plotStats)
import Solution (Solution)
--import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)

import System.Random (getStdGen, setStdGen, mkStdGen)
import Control.Monad (forM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (runReaderT, local)


batch :: Solution r t => Int -> GP [RunStats (r t)]
batch batchSize = forM [1..batchSize] (\i ->
    lift (print i) >> local (newParams i) runGP)


newParams :: Int -> GPParams -> GPParams
newParams i gpp@(GPParams {populationSize = ps}) =
    gpp {populationSize = (fromIntegral i) * ps}


plotBatch :: Int -> GP ()
plotBatch batchSize = do
    concatStatsList <- (batch batchSize :: GP [RunStats ConcatSolution])
    plotStats concatStatsList "../plots/concat"
    --treeStatsList <- (batch batchSize :: GP [RunStats TreeSolution])
    --plotStats treeStatsList "../plots/tree"


params :: GPParams
params = GPParams {
    numGenerations = 10
   ,populationSize = 10
   ,solutionSize = 10
   ,selectionP = 0.3
}


main :: IO ()
main = do
    --setStdGen $ mkStdGen 42
    gen <- getStdGen
    setStdGen gen
    runReaderT (plotBatch 5) $ params
