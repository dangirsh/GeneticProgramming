{-# LANGUAGE MultiParamTypeClasses #-}

module Solution.TreeSolution where


import GP
import Solution
import Common
import Tree
import Term.TreeTerm
import Operator
import Value
import Term

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (asks)


type TreeSolution = Tree TreeTerm


instance Solution Tree TreeTerm where

    randomSol = do
        m <- asks solutionSize
        lift $ randomTree terminals nonTerminals airity (depth m)
        where
            avBranchNum = intAverage $ map airity (nonTerminals :: [TreeTerm])
            depth m = round $ logBase avBranchNum (fromIntegral m)


    evalSol (Node (Terminal (Const x)) _) = return $ Just x
    evalSol (Node (Terminal (Input n)) _) = do
        i <- (!!n)
        return $ Just i
    evalSol (Node (NonTerminal (Op f a _)) children)
        | a == length children = do
            outputs <- sequence (map evalSol children)
            return $ sequence outputs >>= f
        | otherwise = error "Invalid tree."


    crossover sol1 sol2 = do
        node1 <- randomNode sol1
        case filter (nodeCompatible node1) . toNodeList $ sol2 of
            [] -> return sol1
            xs -> do
                    node2 <- randomElem xs
                    return $ swapSubtree node1 node2 sol1
        where
            nodeCompatible (Node x1 _) (Node x2 _) = compatible x1 x2


    mutate = return