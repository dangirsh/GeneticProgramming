{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Solution.ConcatSolution where


import GP
import Common (randomElem, randomInt)
import Solution
import Term
import Term.ConcatTerm
import Operator
import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.List (findIndices)
--import Data.Vector (Vector) -- TODO (swap for list)


type ConcatSolution a = [ConcatTerm a]


instance Eq a => Solution [] ConcatTerm a where

    randomSol = do
        m <- getParam solutionSize
        nt <- nonTerminals
        liftIO $ replicateM m $ randomElem nt

    evalSol [] (top:_) = return top
    evalSol ((MkCT (Op f _ _)):xs) stack = evalSol xs $ f stack
    evalSol _ _ = error "Stack underflow."


    --crossover = twoPtCrossover
    crossover = onePtCrossover
        where
            onePtCrossover sol1 sol2 =
                case sol1 of
                    [] -> randomSol
                    _  -> do
                            i1 <- liftIO . randomInt $ length sol1
                            nInputs <- getReprParam numInputs
                            case splits sol1 sol2 i1 nInputs of
                                [] -> randomSol
                                xs -> do
                                        i2 <- liftIO $ randomElem xs
                                        return $ slice (0, i1) sol1 ++ slice (i2 + 1, length sol2) sol2




    mutate = return


splits :: ConcatSolution a -> ConcatSolution a -> Int -> Int -> [Int]
splits sol1 sol2 i nInputs = findIndices (<=((stackSizes sol1) !! i)) $ stackSizes sol2
    where
        stackSizes = scanl count nInputs
        count c (MkCT (Op _ a _)) = c - a + 1 -- all output airities = 1


slice :: (Int, Int) -> [a] -> [a]
slice (a, b) = take (b - a) . drop a


--onePtCrossover :: ConcatSolution a -> ConcatSolution a -> GP a (ConcatSolution a)
--onePtCrossover sol1 sol2 =
--    case sol1 of
--        [] -> randomSol
--        _  -> do
--                i1 <- liftIO . randomInt $ length sol1
--                nInputs <- length <$> getReprParam inputs
--                case splits sol1 sol2 i1 nInputs of
--                    [] -> randomSol
--                    xs -> do
--                            i2 <- liftIO $ randomElem xs
--                            return $ slice (0, i1) sol1 ++ slice (i2 + 1, length sol2) sol2


--twoPtCrossover :: ConcatSolution a -> ConcatSolution a -> GP a (ConcatSolution a)
--twoPtCrossover sol1 sol2 = do
--    i1A <- randomInt $ length sol1
--    i1B <- randomInt $ length sol1
--    let [splits2A, splits2B] = map (splits sol1 sol2) [i1A, i1B]
--    case [(x, y) | x <- splits2A, y <- splits2B, x < y] of
--        [] -> return sol1
--        xs -> do
--                (i2A, i2B) <- randomElem xs
--                return $ slice (0, i1A) sol1 ++
--                         slice (i2A, i2B + 1) sol2 ++
--                         slice (i1B + 1, length sol1) sol1