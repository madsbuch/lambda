module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/"]
    names = ["L","."]
    style = emptyDef {
                Tok.commentLine = "#"
              , Tok.reservedOpNames = ops
              , Tok.reservedNames = names
              }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer
