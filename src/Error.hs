module Error where

import Ast

data SemantError =
    MisplacedSubdivision Op
  | NegativeBeatValue Literal
  | NegativeRepitition Literal
  | MisplacedBar
  deriving (Show, Eq)
