module Plot where

import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Graphics.Rendering.Chart.Plot.ErrBars (ErrPoint, ErrValue)
import Data.Colour (opaque)
import Data.Colour.Names (green)
import Data.Default.Class (def)
import Control.Lens ((.~), (^.), _2)


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


plotErr :: (PlotValue a, PlotValue b) => [a] -> [(b, b, b)] -> String -> IO ()
plotErr xs y_ranges = _plot [C.toPlot errorBars, C.toPlot averages]
    where
        errorBars = C.plot_errbars_values .~ pts $ def
        averages = C.plot_lines_values .~ [zip xs (map (^._2) y_ranges)] $ def
        pts = zipWith toPt xs y_ranges
        toPt x (yl, ya, yh) = ErrPoint (ErrValue x x x) (ErrValue yl ya yh)


--main = plot "T" "x" "y" "../plots/out.png" [1,2,3,4] [4,3,2,10]