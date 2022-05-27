module Semant where

import Ast
import Error
import Control.Monad.Except
import Control.Monad.ListM
import Control.Monad.State
import Data.Either.Combinators
import Data.List
import Data.Traversable

type Semant = Either SemantError

validOpPosition :: SExpr -> Expr -> Bool
validOpPosition (SOperation prev) (Operation op)
  | op `elem` [Beat, Rest] = True
  | otherwise              = prevPos < opPos
  where
    prevPos = prev `elemIndex` opOrder
    opPos = op `elemIndex` opOrder
    opOrder = [Beat, Rest, SubE, SubAnd, SubA]

checkExprInContext :: SExpr -> Expr -> Semant SExpr
checkExprInContext c (Exprs es) = mapRight (SExprs . snd) (mapAccumM handleCtx c es)
  where
    handleCtx :: SExpr -> Expr -> Semant (SExpr, SExpr)
    handleCtx a b = let x = checkExprInContext a b in mapRight (\x' -> (x',x')) x

checkExprInContext c e@(Operation op) =
  case c of
    (SExprs []) -> Right $ SOperation op
    (SExprs es) -> checkExprInContext (last es) e
    (SOperation _) ->
      if validOpPosition c e
         then Right $ SOperation op
         else Left $ MisplacedSubdivision (opToSubDiv e)

checkProgram :: Program -> Semant SProgram
checkProgram (Program expr) = SProgram <$> checkExprInContext (SExprs []) expr
