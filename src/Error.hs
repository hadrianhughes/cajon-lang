module Error where

data SubdivisionType = E | And | A
                     deriving (Show, Eq)

data SemantError =
    MisplacedSubdivision SubdivisionType
  | NestedBars
  deriving (Show, Eq)
