module GP where


import Common
import Stats (RunStats, getPopStats)
import Solution (Solution)
import Population (Population, nextPop, initPop, sortPop)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans (lift)


evolve :: Solution r t => Int -> Population (r t) -> WriterT (RunStats (r t)) IO (Population (r t))
evolve 0 pop = return pop
evolve i pop = do
            tell [getPopStats pop (g - i + 1)]
            lift (nextPop pop) >>= evolve (i - 1)


runGP :: Solution r t => IO (RunStats (r t))
runGP = do
    (endPop, runStats) <- initPop >>= (runWriterT . evolve g)
    print . head . sortPop $ endPop
    return runStats
