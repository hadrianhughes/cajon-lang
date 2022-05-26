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


-- Semantically valid types

data SLiteral = SLitInt Int
              | SLitNegInt Int
              deriving (Show, Eq)

data SBar = SBar SLiteral SExpr
          deriving (Show, Eq)

data SExpr = SExprs [SExpr]
           | SOperation Op
           | SBars [SBar]
           | SRepitition SLiteral SExpr
           | STempoChange SLiteral
           deriving (Show, Eq)

data SProgram = SProgram SExpr
              deriving (Show, Eq)
