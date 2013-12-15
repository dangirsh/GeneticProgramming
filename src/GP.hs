module GP where


import Common (GPParams, GP, numGenerations)
import Stats (RunStats, getPopStats)
import Solution (Solution)
import Population (Population, nextPop, initPop, sortPop)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans (lift)


evolve :: Solution r t => Int -> Population (r t) -> WriterT (RunStats (r t)) GP (Population (r t))
evolve g = f 0
    where
        f i pop
            | i < g  = do
                tell [getPopStats pop]
                lift (nextPop pop) >>= f (i + 1)
            | otherwise = return pop


runGP :: Solution r t => GP (RunStats (r t))
runGP = do
    g <- asks numGenerations
    (endPop, runStats) <- initPop >>= runWriterT . evolve g
    lift . print . head . sortPop $ endPop
    return runStats