module Semant where

import Ast
import Error
import Control.Monad.Except
import Control.Monad.State

type Semant = Either SemantError

validOpPosition :: [Expr] -> Expr -> Bool
validOpPosition es (Operation op)
  | op `elem` [Beat, Rest] = True
  | op == SubE             = prev `elem` [Beat, Rest]
  | op == SubAnd           = prev `elem` [Beat, Rest, SubE]
  | op == SubA             = prev `elem` [Beat, Rest, SubE, SubAnd]
  where
    (Operation prev) = last es

checkExprs :: [Expr] -> Semant SExpr
checkExprs es = do
  case foldM checkExprs' [] es of
    Left err  -> Left err
    Right es' -> Right $ SExprs $ map (\(Operation op) -> SOperation op) es'
  where
    checkExprs' :: [Expr] -> Expr -> Either SemantError [Expr]
    checkExprs' es e@(Operation op) =
      if validOpPosition es e
         then Right $ es ++ [Operation op]
         else Left $ MisplacedSubdivision (opToSubDiv e)

checkExpr :: Expr -> Semant SExpr
checkExpr (Exprs es) = checkExprs es

checkProgram :: Program -> Semant SProgram
checkProgram (Program expr) = SProgram <$> checkExpr expr
