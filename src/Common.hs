module Common where


import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (genericLength)
import Foreign.Marshal.Utils (fromBool)


--returns a random integer between 0 and i-1, inclusive
randomInt :: Int -> IO Int
randomInt i | i > 0 = randomRIO (0, i - 1)
            | otherwise = error "Integer must be positive."


-- returns a random Double between 0 and f
randDouble :: Double -> IO Double
randDouble f = randomRIO (0.0, f)


-- returns a random element of l
randomElem :: [a] -> IO a
randomElem l =
    case l of
        [] -> error "No elements."
        _  -> do
                i <- randomInt $ length l
                return $ l !! i


-- returns i random elements from l, possibly with duplicates
randomElems :: Int -> [a] -> IO [a]
-- randomElems 0 _ = return []
randomElems i l = replicateM i (randomElem l)


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