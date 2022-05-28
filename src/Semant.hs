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

type InContextChecker = SExpr -> Expr -> Semant SExpr

validOpPosition :: SExpr -> Expr -> Bool
validOpPosition (SOperation prev) (Operation op)
  | op `elem` [Beat, Rest] = True
  | otherwise              = prevPos < opPos
  where
    prevPos = prev `elemIndex` opOrder
    opPos = op `elemIndex` opOrder
    opOrder = [Beat, Rest, SubE, SubAnd, SubA]

checkExprs :: InContextChecker
checkExprs c (Exprs es) = mapRight (SExprs . snd) (mapAccumM handleCtx c es)
  where
    handleCtx :: SExpr -> Expr -> Semant (SExpr, SExpr)
    handleCtx a b = let x = checkExpr a b in mapRight (\x' -> (x',x')) x

checkOperation :: InContextChecker
checkOperation c e@(Operation op) =
  case c of
    (SExprs []) -> Right $ SOperation op
    (SExprs es) -> checkExprs (last es) e
    (SOperation _) ->
      if validOpPosition c e
         then Right $ SOperation op
         else Left $ MisplacedSubdivision op

checkBar :: Bar -> Semant SBar
checkBar b@(Bar v es) =
  case v of
    (LitNegInt _) -> Left $ NegativeBeatValue v
    (LitInt i)    ->
      let ses = checkExprs (SExprs []) es
      in  mapRight (SBar (SLitInt i)) ses

checkBars :: InContextChecker
checkBars c (Bars bs) =
  case c of
    (SExprs []) -> SBars <$> mapM checkBar bs
    _           -> Left $ MisplacedBar

checkExpr :: InContextChecker
checkExpr c e = checker c e
  where
    checker =
      case e of
        (Exprs _)     -> checkExprs
        (Operation _) -> checkOperation
        (Bars _)      -> checkBars

checkProgram :: Program -> Semant SProgram
checkProgram (Program expr) = SProgram <$> checkExpr (SExprs []) expr
