{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import GP (runGP)
import Common
import Stats (RunStats, plotStats)
import Solution (Solution)
import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)

import System.Random (getStdGen, setStdGen, mkStdGen)
import Control.Monad (forM)
import Control.Monad.Trans.Reader (runReaderT)


batch :: Solution r t => Int -> IO [RunStats (r t)]
batch n = forM [0..(n - 1)] (\i -> print i >> (runReaderT runGP $ params))


params :: GPParams
params = GPParams {
    numGenerations = 10
   ,populationSize = 10
   ,solutionSize = 10
   ,selectionP = 0.3

}


main :: IO ()
main = do
    --gen <- getStdGen
    setStdGen $ mkStdGen 42 -- gen
    concatStatsList <- (batch 5 :: IO [RunStats ConcatSolution])
    plotStats concatStatsList "../plots/concat"
    treeStatsList <- (batch 5 :: IO [RunStats TreeSolution])
    plotStats treeStatsList "../plots/tree"