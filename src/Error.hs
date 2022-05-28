module Error where

import Ast

data SemantError =
    MisplacedSubdivision Op
  | NegativeBeatValue Literal
  deriving (Show, Eq)
