{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import Common
import GP (evolve)
import Stats (RunStats, plotStats)
import Solution (Solution, fitness, sizeSol, randomSol)
import Solution.TreeSolution (TreeSolution)
import Solution.ConcatSolution (ConcatSolution)
import Population (Population, initPop, sortPop)


import System.Random (getStdGen, setStdGen, mkStdGen)
import Control.Monad (forM)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Applicative ((<$>))


--runRS :: Solution r t => IO ([r t], [Double])
--runRS = do
--    startPop <- initPop
--    let startSol = startPop !! 0
--    y <- f (n * g) [(startSol, 0)]
--    return $ unzip y
--    where
--        f 0 xs = return xs
--        f i xs@(x:_) = better x >>= \a -> f (i - 1) (a:xs)
--        better (sol, fit) = do
--            new <- randomSol
--            if fitness new > fit then
--                return (new, fitness new)
--            else
--                return (sol, fit)



runGP :: Solution r t => IO (RunStats (r t))
runGP = do
    (endPop, runStats) <- initPop >>= (runWriterT . evolve g)
    print . head . sortPop $ endPop
    return runStats


batch :: Solution r t => Int -> IO [RunStats (r t)]
batch n = forM [0..(n - 1)] (\i -> print i >> runGP)


main :: IO ()
main = do
    --_ <- setStdGen <$> getStdGen
    setStdGen $ mkStdGen 42
    concatStatsList <- (batch 5 :: IO [RunStats ConcatSolution])
    plotStats concatStatsList "../plots/concat"
    treeStatsList <- (batch 5 :: IO [RunStats TreeSolution])
    plotStats treeStatsList "../plots/tree"
    --rs <- runRS :: IO ([ConcatSolution], [Double])g