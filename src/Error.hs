module Error where

import Ast

data SemantError =
    MisplacedSubdivision SExpr Op
  | NegativeBeatValue Literal
  | NegativeRepitition Literal
  | MisplacedBar
  deriving (Show, Eq)

formatOp :: Op -> String
formatOp Beat   = "beat"
formatOp Rest   = "rest"
formatOp SubE   = "'e' subdivision"
formatOp SubAnd = "'+' subdivision"
formatOp SubA   = "'a' subdivision"

formatSemantError :: SemantError -> String
formatSemantError err@(MisplacedSubdivision prev op) =
  case prev of
    (SOperation prevOp) -> "Semant error: " ++ (formatOp op) ++ " cannot be placed after " ++ (formatOp prevOp)
    _ -> "Internal error: " ++ (show err)

formatSemantError (NegativeBeatValue v) = "Semant error: The beat value for a bar cannot be negative: " ++ (show v)

formatSemantError (NegativeRepitition v) = "Semant error: An expression cannot be repeated a negative number of times: " ++ (show v)

formatSemantError MisplacedBar = "Semant error: A bar was misplaced"
