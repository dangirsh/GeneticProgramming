module Value where


data Value a = Const a | Input Int deriving (Show, Eq)


type BoolValue = Value Bool


trueVal :: BoolValue
trueVal = Const True


falseVal :: BoolValue
falseVal = Const False


vals :: [BoolValue]
vals = []
--vals = [trueVal, falseVal]