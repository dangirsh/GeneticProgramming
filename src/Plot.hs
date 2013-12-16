module Plot where

import Stats (PopStats, RunStats, BatchStats, psEvaluations, psFitness, psAvSolSize, psDiversity)
import Common

import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Graphics.Rendering.Chart.Plot.ErrBars (ErrPoint, ErrValue)
import Data.Default.Class (def)
import Control.Lens ((.~), (^.))

import Control.Monad.Trans (lift)
import System.FilePath ((</>))
import Data.List (transpose)


--data MetaData = MetaData {
--    title :: String
--   ,xlabel :: String
--   ,ylabel :: String
--   ,fileName :: String
--}


_plot :: (PlotValue x, PlotValue y) => [Plot x y] -> String -> IO ()
_plot plots fileName = do
    _ <- renderableToFile def (C.toRenderable layout) fileName
    return ()
    where
        layout = C.layout_plots .~ plots $ def
               -- $ C.layout_title .~ title m
               -- $ C.layout_x_axis . C.laxis_title .~ xlabel m
               -- $ C.layout_y_axis . C.laxis_title .~ ylabel m
               -- $ def


plot :: (PlotValue a, PlotValue b) => [a] -> [b] -> String -> IO ()
plot xs ys = _plot [C.toPlot $ C.plot_lines_values .~ [zip xs ys] $ def]


plotErr :: (PlotValue a, PlotValue b) => [ErrValue a] -> [ErrValue b] -> String -> IO ()
plotErr x_errs y_errs = _plot [C.toPlot errorBars, C.toPlot averages]
    where
        errorBars = C.plot_errbars_values .~ pts $ def
        pts = zipWith ErrPoint x_errs y_errs
        averages = C.plot_lines_values .~ [zip (avs x_errs) (avs y_errs)] $ def
        avs [] = []
        avs ((ErrValue _ a _):xs) = a:avs xs


stdDev :: Floating a => [a] -> a
stdDev l = sqrt $ s / fromIntegral (length l)
    where
        a = average l
        s = sum (map (\x -> (x - a) ^ 2) l)


getErr :: (PopStats (r t) -> Double) -> BatchStats (r t) -> ErrValue Double
getErr getStat batchStats = ErrValue low av high
    where
        av = average stats
        low = av - stdErr
        high = av + stdErr
        stats = map getStat batchStats
        batchSize = length batchStats
        stdErr = stdDev stats / sqrt (fromIntegral batchSize)


plotStat :: [RunStats (r t)] -> String -> ((PopStats (r t) -> Double), String) -> GP ()
plotStat statsList prefix (getStat, statName) =
    lift $ plotErr x_errs y_errs (prefix </> statName ++ ".png")
    where
        x_errs = scanl a (ErrValue 0 0 0) $ f psEvaluations
        y_errs = f getStat
        f g = map (getErr g) $ transpose statsList
        a (ErrValue _ la _) (ErrValue tl ta th) = (ErrValue (tl + la) (ta + la) (th + la))

plotStats :: [RunStats (r t)] -> String -> GP ()
plotStats statsList prefix = mapM_ (plotStat statsList prefix) toPlot
    where
        toPlot = [(psFitness, "Fitness")
                 ,(psAvSolSize, "Size")
                 ,(psDiversity, "Diversity")]

--main = plot "T" "x" "y" "../plots/out.png" [1,2,3,4] [4,3,2,10]