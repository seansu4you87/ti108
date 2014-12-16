module TI108
(
) where

import Text.Parsec (ParseError)

import TI108.Operation
import qualified TI108.Parser as Parser

evaluate :: String -> Either ParseError Float
evaluate string = case Parser.test Parser.operation string of
                     Left x -> Left x
                     Right op -> Right (calculateOperation op)
