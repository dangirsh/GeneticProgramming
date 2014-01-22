{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Solution.TreeSolution where


import GP
import Solution
import Common
import Tree
import Term.TreeTerm
import Operator
import Value
import Term

import Control.Monad.Trans (liftIO)


type TreeSolution a = Tree (TreeTerm a)


instance (Eq a, Show a) => Solution Tree TreeTerm a where

    randomSol = do
        m <- getParam solutionSize
        ts <- terminals
        nts <- nonTerminals
        liftIO $ randomTree ts nts airity (toDepth m nts)
        where
            avBranchNum nts = intAverage $ map airity nts
            toDepth m nts = round $ logBase (avBranchNum nts) (fromIntegral m)


    evalSol (Node (Terminal (Const x)) _) _ = return $ x
    evalSol (Node (Terminal (Input n)) _) input = return $ input !! n
    evalSol (Node (NonTerminal (Op f a _)) children) input
        | a == length children = do
            outputs <- mapM (\c -> evalSol c input) children
            return $ f outputs
        | otherwise = error "Invalid tree."


    crossover sol1 sol2 = do
        node1 <- liftIO . randomNode $ sol1
        case filter (nodeCompatible node1) . toNodeList $ sol2 of
            [] -> return sol1
            xs -> do
                    node2 <- liftIO . randomElem $ xs
                    return $ swapSubtree node1 node2 sol1
        where
            nodeCompatible (Node x1 _) (Node x2 _) = compatible x1 x2


    mutate = return