module TI108.Parser
( test
, nested
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
operator = plus <|> minus <|> star <|> slash

plus :: Parsec String () Operator
plus = (\_ -> Add) <$> char '+'

minus :: Parsec String () Operator
minus = (\_ -> Sub) <$> char '-'

star :: Parsec String () Operator
star = (\_ -> Mul) <$> char '*'

slash :: Parsec String () Operator
slash = (\_ -> Div) <$> char '/'

openParen :: Parsec String () Char
openParen = char '('

closeParen :: Parsec String () Char
closeParen = char '('

operation :: Parsec String () Operation
operation = do
    op <- singleOperation <|> singleParentheticalOperation
    spaces
    moreOps <- many additionalOperation
    return $ foldl reducer op moreOps
        where reducer acc opFun = opFun acc

singleOperation :: Parsec String () Operation
singleOperation = do
    a <- operand
    spaces
    op <- operator
    spaces
    b <- (Value <$> operand) <|> singleParentheticalOperation
    return $ Operation op (Value a) b

singleParentheticalOperation :: Parsec String () Operation
singleParentheticalOperation = do
    char '('
    spaces
    op <- operation
    spaces
    char ')'
    return op

additionalOperation :: Parsec String () (Operation -> Operation)
additionalOperation = do
    op <- operator
    spaces
    c <- (Value <$> operand) <|> singleParentheticalOperation
    spaces
    return $ \ab -> Operation op ab c
