module Error where

import Ast

data SemantError =
    MisplacedSubdivision Op
  | NegativeBeatValue Literal
  | MisplacedBar
  deriving (Show, Eq)
