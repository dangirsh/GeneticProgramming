{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import GP
import Stats (RunStats, getPopStats, psBest)
import Plot (plotStats)
import Solution (Solution)
import Population (nextPop, initPop, sortPop)
import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)
import Operator (ops)
import Value (vals)

import System.Random (setStdGen, mkStdGen)
--import System.Random (getStdGen)
import Control.Applicative ((<$>))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Foreign.Marshal.Utils (fromBool)


evolve :: Solution r t a => GP a (RunStats (r (t a)))
evolve = do
    g <- getParam numGenerations
    (best, stats) <- runWriterT $ foldl f (lift initPop) [1..g]
    top <- head <$> sortPop best
    lift . print $ top
    return stats
    where
        f acc _ = do
            pop <- acc
            stats <- lift $ getPopStats pop
            --liftIO . print $ psBest stats
            tell [stats]
            lift (nextPop pop)


batch :: Solution r t a => Int -> GPParams a -> IO [RunStats (r (t a))]
batch batchSize p =
    forM [1..batchSize] (\i -> print i >> runGP evolve p)


plotBatch :: Int -> GPParams Bool -> IO ()
plotBatch batchSize p = do
    --concatStatsList <- (batch batchSize p :: IO [RunStats (ConcatSolution Bool)])
    --plotStats concatStatsList "../plots/concat"
    treeStatsList <- (batch batchSize p :: IO [RunStats (TreeSolution Bool)])
    plotStats treeStatsList "../plots/tree"
    return ()



gpParams :: GPParams Bool
gpParams = GPParams {
    numGenerations = 30
   ,populationSize = 30
   ,solutionSize = 30
   ,selectionP = 0.3
   ,crossoverP = 0.9
   ,mutationP = 0.1
   ,reprParams = ReprParams {
        numInputs = 4
       ,inputs = replicateM (numInputs (reprParams gpParams)) [True, False]
       ,trueSolution = even . sum . map fromBool
       ,values = vals
       ,operators = ops
    }
}


main :: IO ()
--main = setStdGen <$> getStdGen >> plotBatch 3 gpParams
main = setStdGen (mkStdGen 42) >> plotBatch 1 gpParams
