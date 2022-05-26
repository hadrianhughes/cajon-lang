{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Monad (void)
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "#-" "-#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

slashes :: Parser a -> Parser a
slashes = between (single '/') (single '/')

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

dot :: Parser ()
dot = void $ symbol "."

comma :: Parser ()
comma = void $ symbol ","

plus :: Parser ()
plus = void $ symbol "+"

echar :: Parser ()
echar = void $ symbol "e"

achar :: Parser ()
achar = void $ symbol "a"

colon :: Parser ()
colon = void $ symbol ":"

int :: Parser Int
int = lexeme L.decimal

dash :: Parser ()
dash = void $ symbol "-"
