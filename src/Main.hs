{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import Common
import GP (evolve, runGP)
import Stats (RunStats, plotStats)
import Solution (Solution)
import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)

import System.Random (getStdGen, setStdGen, mkStdGen)
import Control.Monad (forM)


batch :: Solution r t => Int -> IO [RunStats (r t)]
batch n = forM [0..(n - 1)] (\i -> print i >> runGP)


main :: IO ()
main = do
    --gen <- getStdGen
    setStdGen $ mkStdGen 42 -- gen
    concatStatsList <- (batch 5 :: IO [RunStats ConcatSolution])
    plotStats concatStatsList "../plots/concat"
    treeStatsList <- (batch 5 :: IO [RunStats TreeSolution])
    plotStats treeStatsList "../plots/tree"
    --rs <- runRS :: IO ([ConcatSolution], [Double])g