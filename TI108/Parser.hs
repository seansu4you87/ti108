module TI108.Parser
( test
, operation
) where

import Text.Parsec
import Control.Applicative hiding ((<|>), many)

import TI108.Operation

-- Testing
test rule text = parse rule "(source)" text
nested = "(1 + (2 * 3)) - (4 / 5)"

-- Parsers

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
    op <- operator
    spaces
    c <- operand
    spaces
    return $ \ab -> Operation op ab (Value c)

operation :: Parsec String () Operation
operation = do
    op <- singleOperation
    spaces
    moreOps <- many additionalOperation
    return $ foldl reducer op moreOps
        where reducer acc opFun = opFun acc

singleParentheticalOperation :: Parsec String () Operation
singleParentheticalOperation = do
    char '('
    spaces
    op <- operation
    spaces
    char ')'
    return op

additionalParentheticalOperation :: Parsec String () (Operation -> Operation)
additionalParentheticalOperation = do
    op <- operator
    spaces
    c <- singleParentheticalOperation
    spaces
    return $ \ab -> Operation op ab c

parentheticalOperation :: Parsec String () Operation
parentheticalOperation = do
    op <- (operation <|> singleParentheticalOperation)
    spaces
    moreOps <- many (try additionalParentheticalOperation <|> additionalOperation)
    return $ foldl reducer op moreOps
        where reducer acc opFun = opFun acc
