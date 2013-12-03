module Term.Boolean (boolTerms) where


import Common
import Term


boolTerms = map Terminal vals ++ map NonTerminal ops


trueVal :: Value
trueVal = Const True


falseVal :: Value
falseVal = Const False


constants :: [Value]
constants = [trueVal, falseVal]


vals :: [Value]
vals = constants ++ map Input [0..(numInputs - 1)]


notOp :: Op
notOp = Op op 1 "not"
    where
        op (x:[]) = Just (not x)
        op _ = Nothing


binOp :: (Bool -> Bool -> Bool, String) -> Op
binOp (f, name) = Op op 2 name
    where
        op (x1:x2:[]) = Just (f x1 x2)
        op _ = Nothing


ops :: [Op]
ops = notOp : map binOp [((&&), "and"), ((||), "or"), ((/=), "xor")]