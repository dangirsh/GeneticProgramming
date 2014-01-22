module Stats where

import GP
import Common (average, intAverage)
import Solution (Solution, fitness, sizeSol)
import Population (Population, sortPop, getDiversity)

import Control.Applicative ((<$>))



data PopStats a =
    PopStats {
         psEvaluations::Double
        ,psFitness::Double
        ,psAvFitness::Double
        ,psAvSolSize::Double
        ,psDiversity::Double
        ,psBest::String
    }


type RunStats a = [PopStats a]


type BatchStats a = [PopStats a]


getPopStats :: (Solution r t a, Eq a) => Population (r (t a)) -> GP a (PopStats (r (t a)))
getPopStats pop = do
    best <- head <$> sortPop pop
    f <- fitness best
    avF <- average <$> mapM fitness pop
    return $ PopStats {
         psEvaluations = fromIntegral . length $ pop
        ,psFitness = f
        ,psAvFitness = avF
        ,psAvSolSize =intAverage . map sizeSol $ pop
        ,psDiversity = getDiversity pop
        ,psBest = show best
    }