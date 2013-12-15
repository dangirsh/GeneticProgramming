module GP where


import Common (GPParams, GP, numGenerations)
import Stats (RunStats, getPopStats)
import Solution (Solution)
import Population (Population, nextPop, initPop, sortPop)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans (lift)


evolve :: Solution r t => Int -> Population (r t) -> WriterT (RunStats (r t)) GP (Population (r t))
evolve 0 pop = return pop
evolve i pop = do
    g <- lift $ asks numGenerations
    tell [getPopStats pop (g - i + 1)]
    (lift . lift) (nextPop pop) >>= evolve (i - 1)


runGP :: Solution r t => GP (RunStats (r t))
runGP = do
    g <- asks numGenerations
    (endPop, runStats) <- initPop >>= runWriterT . evolve g
    lift . print . head . sortPop $ endPop
    return runStats