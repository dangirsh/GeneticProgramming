{-# LANGUAGE MultiParamTypeClasses #-}

module Term where

import GP
import Value (Value)
import Operator (BaseOp)

-- TODO: really should be split into TermSet and Term
--class (Show (t a), Eq (t a)) => Term t a where
class Term t where
    compatible :: t a -> t a -> Bool
    terminals :: (GP a) [t a]
    nonTerminals :: (GP a) [t a]
    airity :: t a -> Int
    convertVal :: Value a -> t a
    convertOp :: BaseOp a -> t a