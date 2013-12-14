module Value where

import Common

data Value a = Const a | Input Int deriving (Show, Eq)


type TreeValue = Value ValueType


trueVal :: TreeValue
trueVal = Const True


falseVal :: TreeValue
falseVal = Const False


consts :: [TreeValue]
--consts = [trueVal, falseVal]
consts = []
