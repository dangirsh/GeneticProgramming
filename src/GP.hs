module GP where


import Common (GPParams, GP, numGenerations)
import Stats (RunStats, getPopStats)
import Solution (Solution)
import Population (Population, nextPop, initPop, sortPop)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.Trans (lift)


evolve :: Solution r t => WriterT (RunStats (r t)) GP (Population (r t))
evolve = do
    g <- lift $ asks numGenerations
    foldl f (lift initPop) [1..g]
    where
        f acc generation = do
            pop <- acc
            tell [getPopStats pop]
            lift (nextPop pop)


runGP :: Solution r t => GP (RunStats (r t))
runGP = do
    (endPop, runStats) <- runWriterT evolve
    lift . print . head . sortPop $ endPop
    return runStats