import Text.Parsec
import Control.Applicative hiding ((<|>), many)

p rule text = parse rule "(source)" text

type Operand = Float
data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data Operation = Value Operand | Operation Operator Operation Operation deriving (Show)

operand :: Parsec String () Operand
operand = readOperand <$> number
    where readOperand = read :: String -> Float
          number      = many1 digit

operator :: Parsec String () Operator
operator = readOperator <$> (plus <|> minus <|> star <|> slash)
    where plus  = char '+'
          minus = char '-'
          star  = char '*'
          slash = char '/'
          readOperator x | x == '+' = Add
                         | x == '-' = Sub
                         | x == '*' = Mul
                         | x == '/' = Div

singleOperation :: Parsec String () Operation
singleOperation = do
  a <- operand
  spaces
  op <- operator
  spaces
  b <- operand
  return $ Operation op (Value a) (Value b)

additionalOperation :: Parsec String () (Operation -> Operation)
additionalOperation = do
  spaces
  op <- operator
  spaces
  c <- operand
  return $ \ab -> Operation op ab (Value c)

operation :: Parsec String () Operation
operation = do
  op <- singleOperation
  moreOps <- many additionalOperation
  return $ foldl reducer op moreOps
      where reducer acc opFun = opFun acc

calculateOperation :: Operation -> Operand
calculateOperation (Value a) = a
calculateOperation (Operation op a b) =
    case op of
      Add -> calculateOperation a + calculateOperation b
      Sub -> calculateOperation a - calculateOperation b
      Mul -> calculateOperation a * calculateOperation b
      Div -> calculateOperation a / calculateOperation b

calculate :: String -> Either ParseError Float
calculate string = case p operation string of
                     Left x -> Left x
                     Right op -> Right (calculateOperation op)
