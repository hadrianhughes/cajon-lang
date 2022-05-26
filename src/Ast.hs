module Ast where

data Op = Beat
        | Rest
        | SubE
        | SubAnd
        | SubA
        deriving (Show, Eq)

data TempoChange = TempoUp Literal
                 | TempoDown Literal
                 deriving (Show, Eq)

data Literal = LitInt Int
             | LitNegInt Int
             deriving (Show, Eq)

data Expr = Exprs [Expr]
          | Operation Op
          | Bar Literal Expr
          | Repitition Literal Expr
          deriving (Show, Eq)

data Program = Program Expr
          deriving (Show, Eq)
