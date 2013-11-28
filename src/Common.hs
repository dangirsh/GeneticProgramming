module Common (
    randInt,
    randDouble,
    randElem,
    random,
    average,
    intAverage,
    every,
    plot,
    g, n, m, selection_p, k, crossover_p, mutation_p
) where


import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (genericLength)

-- imports for plot
import Graphics.Rendering.Chart as C
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Backend.Cairo (renderableToFile)
import Data.Colour (opaque)
import Data.Colour.Names (green)
import Data.Default.Class (def)
import Control.Lens ((.~))


--returns a random integer between 0 and i-1, inclusive
randInt :: Int -> IO Int
randInt i = randomRIO (0, i - 1)


-- returns a random Double between 0 and f
randDouble :: Double -> IO Double
randDouble f = randomRIO (0.0, f)


-- returns a random element of l
randElem :: [a] -> IO a
randElem l = do
    i <- randInt $ length l
    return $ l !! i


-- returns i random elements from l, possibly with duplicates
randElems :: Int -> [a] -> IO [a]
-- randElems 0 _ = return []
randElems i l = replicateM i (randElem l)


random :: IO Double
random = randDouble 1.0 :: IO Double


average :: Fractional a => [a] -> a
average l = sum l / genericLength l


intAverage :: Integral a => Fractional b => [a] -> b
intAverage = average . map fromIntegral


every :: [a] -> Int -> [a]
every xs i = case drop (i-1) xs of
              (y:ys) -> y : every ys i
              [] -> []


-- number of generations
g :: Int
g = 100

-- size of population
n :: Int
n = 100

-- size of solution
m :: Int
m = 100

-- fraction to select
selection_p :: Double
selection_p = 0.50

-- number to select
k :: Int
k = round $ (fromIntegral n) * selection_p

-- probability of crossover
crossover_p :: Double
crossover_p = 0.5

-- probability of mutation
mutation_p :: Double
mutation_p = 0.2 :: Double


plot :: PlotValue a => PlotValue b => String -> String -> String -> String -> [a] -> [b] -> IO ()
plot title xlabel ylabel fname xs ys = do
    _ <- renderableToFile def (C.toRenderable layout) fname
    return ()
    where
        layout = C.layout_plots .~ [C.toPlot p]
                $ C.layout_title .~ title
                $ C.layout_x_axis . C.laxis_title .~ xlabel
                $ C.layout_y_axis . C.laxis_title .~ ylabel
                $ def
        p = C.plot_lines_values .~ [zip xs ys] $ def


--main = plot "T" "x" "y" "../plots/out.png" [1,2,3,4] [4,3,2,10]