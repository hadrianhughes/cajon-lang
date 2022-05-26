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

operationP :: Parser Expr
operationP = Operation Beat <$ dot
         <|> Operation Rest <$ comma
         <|> Operation SubE <$ echar
         <|> Operation SubAnd <$ plus
         <|> Operation SubA <$ achar

litP :: Parser Literal
litP = LitNegInt <$> (dash *> int)
   <|> LitInt <$> int

barP :: Parser Expr
barP = try (Bar <$> (single '/' *> litP) <* colon <*> (Exprs <$> someTill exprP (single '/')))
   <|> Bar (LitInt 4) <$> Exprs <$> (single '/' *> someTill exprP (single '/'))

tempoP :: Parser Expr
tempoP = TempoChange <$> angles litP

exprP :: Parser Expr
exprP = Exprs <$> parens (some exprP)
    <|> operationP
    <|> barP
    <|> tempoP
    <|> try (Repitition <$> litP <*> exprP)

programP :: Parser Program
programP = between sc eof (Program <$> exprP)
