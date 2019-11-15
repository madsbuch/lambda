module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Lambda

type Ctx = [(String, Int)]

genCtx :: Term -> Ctx
genCtx term = []

-- Increments all elements by one
ctxIncr :: Ctx -> Ctx
ctxIncr ctx = map (\(x, i) -> (x, i+1)) ctx

lookupCtx :: Ctx -> String -> Maybe Int
lookupCtx [] x = Nothing
lookupCtx ((x, i) : r) x' 
  | x == x' = Just i
  | otherwise = lookupCtx r x'

variable :: Ctx -> Parser Term
variable ctx = do
  var <- identifier
  let i = case lookupCtx ctx var of Just i -> i
                                    Nothing -> -1
  return $ TmVar (OrigName var) i

abstraction :: Ctx -> Parser Term
abstraction ctx = do
  (char '\\') <|> (char 'λ')
  name <- identifier
  char '.'

  let ctx' = ctxIncr ctx
  let ctx'' = (name, 0) : ctx'
  body <- parseTerm ctx''
  return $ TmAbs Info name body

nonAppTerm :: Ctx -> Parser Term
nonAppTerm ctx = (abstraction ctx)
          <|> (variable ctx)
          <|> parens (parseTerm ctx)

parseTerm :: Ctx -> Parser Term
parseTerm ctx = chainl1 (nonAppTerm ctx) $ do
                  return $ TmApp Info

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

doParseTerm :: String -> Either ParseError Term
doParseTerm s = parse (contents (parseTerm [])) "<stdin>" s

doParse s = case doParseTerm s of
  Right a -> a
  _ -> error "Can not parse"
