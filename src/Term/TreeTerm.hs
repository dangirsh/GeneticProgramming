module Term.TreeTerm where

import Term
import Operator
import Value
import Common


data TreeTerm = Terminal (Value ValueType)
              | NonTerminal (Op InputType ValueType) deriving (Show, Eq)


instance Term TreeTerm where
    compatible (Terminal _) (Terminal _) = True
    compatible (NonTerminal (Op _ a1 _)) (NonTerminal (Op _ a2 _)) = a1 == a2
    compatible _ _ = False
    terminals = map Terminal $ consts ++ map Input [0..(numInputs - 1)]
    nonTerminals = map NonTerminal ops
    airity (Terminal _) = 0
    airity (NonTerminal (Op _ a _)) = a
