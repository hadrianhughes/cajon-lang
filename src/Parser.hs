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

barP :: Parser Expr
barP = try (Bar <$> (single '/' *> int) <* colon <*> (Exprs <$> someTill exprP (single '/')))
   <|> Bar 4 <$> Exprs <$> (single '/' *> someTill exprP (single '/'))

exprP :: Parser Expr
exprP = Exprs <$> parens (some exprP)
    <|> Operation Beat <$ dot
    <|> Operation Rest <$ comma
    <|> Operation SubE <$ echar
    <|> Operation SubAnd <$ plus
    <|> Operation SubA <$ achar
    <|> Neg <$> (dash *> int)
    <|> barP
    <|> try (Repitition <$> int <*> exprP)

programP :: Parser Program
programP = between sc eof (Program <$> exprP)
