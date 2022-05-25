module Ast where

data Op = Beat
        | Rest
        | SubE
        | SubAnd
        | SubA
        | TempoChange
        deriving (Show, Eq)

data Bar = Bar Int Expr
        deriving (Show, Eq)

data Expr = Operations [Op]
          | Bars [Bar]
          | Repitition Int Expr
          | Noexpr
          deriving (Show, Eq)

data Program = Program Expr
          deriving (Show, Eq)
