module Stats where

import Common
import Solution (Solution, fitness, sizeSol)
import Population (Population, sortPop, getDiversity)
import System.FilePath ((</>))
import Plot (plotErr)

import Data.List (transpose)
--import Control.Monad (forM)


data PopStats a =
    PopStats {
         psFitness::Double
        ,psAvFitness::Double
        ,psAvSolSize::Double
        ,psDiversity::Double
    } deriving Show


type RunStats a = [PopStats a]


type BatchStats a = [PopStats a]


getPopStats :: Solution r t => Population (r t) -> Int -> PopStats (r t)
getPopStats pop i =
    PopStats {
         psFitness = fitness . head . sortPop $ pop
        ,psAvFitness = average . map fitness $ pop
        ,psAvSolSize =intAverage . map sizeSol $ pop
        ,psDiversity = getDiversity pop
    }


stdDev :: Floating a => [a] -> a
stdDev l = sqrt $ x / fromIntegral (length l)
    where
        a = average l
        x = sum (map (\x -> (x - a) ^ 2) l)


getRange :: (PopStats (r t) -> Double) -> BatchStats (r t) -> (Double, Double, Double)
getRange getStat batchStats = (low, av, high)
    where
        av = average stats
        low = av - stdErr
        high = av + stdErr
        stats = map getStat batchStats
        stdErr = stdDev stats / sqrt (fromIntegral n)


plotStat :: [RunStats (r t)] -> String -> ((PopStats (r t) -> Double), String) -> IO ()
plotStat statsList prefix (getStat, statName) =
    plotErr xs y_ranges (prefix </> statName)
    where
        xs = [0,n..g*n]
        y_ranges = map (getRange getStat) $ transpose statsList


plotStats :: Solution r t => [RunStats (r t)] -> String -> IO ()
plotStats statsList prefix = mapM_ (plotStat statsList prefix) toPlot
    where
        toPlot = [(psFitness, "Fitness")
                 ,(psAvSolSize, "Size")
                 ,(psDiversity, "Diversity")]