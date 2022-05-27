module Error where

import Ast

data SubdivisionType = E | And | A
                     deriving (Show, Eq)

opToSubDiv :: Expr -> SubdivisionType
opToSubDiv (Operation op) =
  case op of
    SubE    -> E
    SubAnd  -> And
    SubA    -> A
    _       -> error ("Can't map " ++ (show op) ++ " to a SubdivisionType")

data SemantError =
    MisplacedSubdivision SubdivisionType
  | NestedBars
  deriving (Show, Eq)