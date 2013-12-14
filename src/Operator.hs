module Operator where


import Data.Maybe (catMaybes)
import Common


data Op a b = Op (a -> Maybe b) Int String


instance Eq (Op a b) where
    Op _ airity1 name1 == Op _ airity2 name2 =
        airity1 == airity2 && name1 == name2


instance Show (Op a b) where
    show (Op _ _ name) = show name


type TreeOp = Op [ValueType] ValueType


notOp :: TreeOp
notOp = Op op 1 "not"
    where
        op (x:xs) = Just (not x)
        op _ = Nothing


binOp :: (Bool -> Bool -> Bool, String) -> TreeOp
binOp (f, name) = Op op 2 name
    where
        op (x1:x2:xs) = Just (f x1 x2)
        op _ = Nothing


ops :: [TreeOp]
ops = notOp : map binOp [((&&), "and"), ((||), "or"), ((/=), "xor")]
                         --,((\a b -> (not (a && b))), "nand")
                         --,((\a b -> (not (a || b))), "nor")
                        --]