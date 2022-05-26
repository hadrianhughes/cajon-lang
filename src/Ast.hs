module Ast where

data Op = Beat
        | Rest
        | SubE
        | SubAnd
        | SubA
        deriving (Show, Eq)

data TempoChange = TempoUp Int
                 | TempoDown Int
                 deriving (Show, Eq)

data Expr = Exprs [Expr]
          | Operation Op
          | Bar Int Expr
          | Repitition Int Expr
          | Neg Int
          deriving (Show, Eq)

data Program = Program Expr
          deriving (Show, Eq)
