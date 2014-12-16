module TI108.Operation
( Operand
, Operator(Add,Sub,Mul,Div)
, Operation(Value,Operation)
, calculateOperation
) where

type Operand = Float
data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data Operation = Value Operand | Operation Operator Operation Operation deriving (Show)

calculateOperation :: Operation -> Operand
calculateOperation (Value a) = a
calculateOperation (Operation op a b) =
    case op of
      Add -> calculateOperation a + calculateOperation b
      Sub -> calculateOperation a - calculateOperation b
      Mul -> calculateOperation a * calculateOperation b
      Div -> calculateOperation a / calculateOperation b
