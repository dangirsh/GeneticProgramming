{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}


module GP where


import Value
import Operator

import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Applicative ((<$>))


data GPParams a = GPParams {
        numGenerations :: Int
       ,populationSize :: Int
       ,solutionSize :: Int
       ,selectionP :: Double
       ,crossoverP :: Double
       ,mutationP :: Double
       ,reprParams :: ReprParams a
    }


data ReprParams a = ReprParams {
        numInputs :: Int
       ,inputs :: [[a]]
       ,trueSolution :: [a] -> a
       ,values :: [Value a]
       ,operators :: [BaseOp a]
    }


getParam :: (GPParams a -> b) -> (GP a) b
getParam = asks


getReprParam :: (ReprParams a -> b) -> (GP a) b
getReprParam f = f <$> asks reprParams


type GP a = ReaderT (GPParams a) IO


runGP :: (GP a) b -> (GPParams a) -> IO b
runGP gp params = runReaderT gp $ params
