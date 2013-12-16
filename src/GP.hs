module GP where


import Control.Monad.Trans.Reader (ReaderT, runReaderT)


data GPParams = GPParams {
    numGenerations :: Int
   ,populationSize :: Int
   ,solutionSize :: Int
   ,selectionP :: Double
}


type GP = ReaderT GPParams IO


runGP :: GP a -> GPParams -> IO a
runGP gp params = runReaderT gp $ params
