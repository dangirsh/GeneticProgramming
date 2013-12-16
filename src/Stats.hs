module Stats where

import Common (average, intAverage)
import Solution (Solution, fitness, sizeSol)
import Population (Population, sortPop, getDiversity)


data PopStats a =
    PopStats {
         psEvaluations::Double
        ,psFitness::Double
        ,psAvFitness::Double
        ,psAvSolSize::Double
        ,psDiversity::Double
    } deriving Show


type RunStats a = [PopStats a]


type BatchStats a = [PopStats a]


getPopStats :: Solution r t => Population (r t) -> PopStats (r t)
getPopStats pop =
    PopStats {
         psEvaluations = fromIntegral . length $ pop
        ,psFitness = fitness . head . sortPop $ pop
        ,psAvFitness = average . map fitness $ pop
        ,psAvSolSize =intAverage . map sizeSol $ pop
        ,psDiversity = getDiversity pop
    }