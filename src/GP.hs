module GP where


import Common
import Stats (RunStats, getPopStats)
import Solution (Solution)
import Population (Population, nextPop)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Trans (lift)


evolve :: Solution r t => Int -> Population (r t) -> WriterT (RunStats (r t)) IO (Population (r t))
evolve 0 pop = return pop
evolve i pop = do
            tell [getPopStats pop (g - i + 1)]
            lift (nextPop pop) >>= evolve (i - 1)