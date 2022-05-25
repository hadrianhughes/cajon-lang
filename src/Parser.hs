{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( programP
  , runParser
  )
  where

import Ast
import Lexer
import Text.Megaparsec
import Control.Monad.Combinators.Expr

exprP :: Parser Expr
exprP = parens exprP
    <|> angles exprP
    <|> Operation Beat <$ dot
    <|> Operation Rest <$ comma
    <|> Operation SubE <$ echar
    <|> Operation SubAnd <$ plus
    <|> Operation SubA <$ achar
    <|> Neg <$> (dash *> int)
    <|> try (Bar <$> int <* colon *> angles exprP)
    <|> Bar 4 <$> angles exprP
    <|> try (Repitition <$> int <*> exprP)
    <|> Literal <$> int

programP :: Parser Program
programP = between sc eof (Program <$> rootExpr)
  where
    rootExpr = Exprs <$> many exprP
