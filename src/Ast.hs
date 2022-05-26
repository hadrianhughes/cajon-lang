module Ast where

data Op = Beat
        | Rest
        | SubE
        | SubAnd
        | SubA
        deriving (Show, Eq)

data Literal = LitInt Int
             | LitNegInt Int
             deriving (Show, Eq)

data Bar = Bar Literal Expr
         deriving (Show, Eq)

data Expr = Exprs [Expr]
          | Operation Op
          | Bars [Bar]
          | Repitition Literal Expr
          | TempoChange Literal
          deriving (Show, Eq)

data Program = Program Expr
          deriving (Show, Eq)
