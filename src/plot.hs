import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Data.Colour (opaque)
import Data.Colour.Names (green)
import Data.Default.Class (def)
import Control.Lens ((.~))


plot :: String -> String -> String -> String -> [Int] -> [Int] -> IO (PickFn ())
plot title xlabel ylabel fname xs ys = renderableToFile def (C.toRenderable layout) fname
    where
        layout = C.layout_plots .~ [C.toPlot p]
                $ C.layout_title .~ title
                $ C.layout_x_axis . C.laxis_title .~ xlabel
                $ C.layout_y_axis . C.laxis_title .~ ylabel
                $ def
        p = C.plot_lines_values .~ [zip xs ys] $ def


main :: IO ()
main = do
    _ <- plot "T" "x" "y" "../plots/test.png" [1,2,3,4] [4,3,2,10]
    return ()