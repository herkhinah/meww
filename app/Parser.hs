{-# OPTIONS -Wno-unused-imports #-}
module Parser where

import Common (Name, Parser)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Functor.Identity qualified
import Data.Void
import Syntax
import Syntax.Builtin qualified as B
import Syntax.Data qualified as D
import Syntax.Term qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

{-
withPos :: Parser Tm -> Parser Tm
withPos p = SrcPos <$> getSourcePos <*> p
-}

lexeme = L.lexeme ws

symbol s = lexeme (C.string s)

char c = lexeme (C.char c)

parens p = char '(' *> ws *> p <* ws <* char ')'

braces p = char '{' *> p <* char '}'

pArrow = symbol "→" <|> symbol "->"

pKeyword :: String -> Parser ()
pKeyword kw = do
  C.string kw <* ws
  pure ()

pList :: Parser a -> Parser [a]
pList p = parens (many p)

pIdent :: Parser String
pIdent = do
  ident <- takeWhile1P Nothing isAlphaNum <* ws
  guard (isAlpha (head ident) && ident /= "lam")
  pure ident

pStringLit :: Parser String
pStringLit = char '"' *> takeWhile1P Nothing (/= '"') <* char '"'

pFloatLit :: Parser Float
pFloatLit = do
  str <-
    takeWhile1P Nothing isNumber
      <> C.string "."
      <> takeWhileP
        Nothing
        isNumber
  pure $ read str

pAtom :: Parser T.Tm
pAtom = try (fmap T.PrimOp pPrimOp) <|> try (fmap T.Var pIdent) <|> (try pTm <|> T.Void <$ ws) <* ws

pTm :: Parser T.Tm
pTm =
  parens $
    try pLet <|> try pApp <|> pAtom

pLet :: Parser T.Tm
pLet = do
  pKeyword "let"
  defs <- try (fmap pure pDef) <|> parens (many pDef)
  tm <- pAtom
  pure $ foldr (\(nm, def) tm -> T.Let nm def tm) tm defs
  where
    pDef :: Parser (Name, T.Tm)
    pDef = parens $ do
      ident <- pIdent
      tm <- pAtom
      pure (ident, tm)

pLam :: Parser T.Tm
pLam = parens $ do
  pKeyword "lam"
  args <- fmap pure pIdent <|> parens (some pIdent)
  T.Lam args <$> pAtom

pPrimOp :: Parser B.PrimOp
pPrimOp =
  ( C.string "+" $> B.Plus
      <|> C.string "-" $> B.Minus
      <|> C.string "*" $> B.Mult
      <|> C.string "print" $> B.Print
  )
    <* ws

pApp :: Parser T.Tm
pApp = do
  rator <- pAtom
  rand <- some pAtom
  pure $ T.App rator rand

pFun :: Parser Fun
pFun = do
  pKeyword "fun"
  name <- pIdent
  args <- pList pIdent
  body <- pTm
  pure Syntax.Fun {name = name, args = args, body = body}

pTypevar :: Parser Name
pTypevar = fmap pure (char '\'') <> pIdent

pData :: Parser D.Data
pData = do
  pKeyword "data"
  name <- pIdent
  args <- optional $ pList pTypevar
  cons <- many (parens pCons)
  pure D.Data {name = name, args = args, cons = cons}

pCons :: Parser D.Cons
pCons = do
  name <- pIdent
  args <- many pConsArg
  pure D.Cons {name = name, args = args}

pConsArg :: Parser D.ConsArg
pConsArg = fmap D.TypeVar pTypevar <|> fmap D.Con pIdent <|> parens (fmap D.App pIdent <*> some pConsArg)

pConsArgAtom :: Parser D.ConsArg
pConsArgAtom = try (fmap D.TypeVar pTypevar) <|> try (fmap D.Con pIdent) <|> pApplied
  where
    pApplied :: Parser D.ConsArg
    pApplied = parens $ do
      name <- pIdent
      args <- many (fmap D.TypeVar pTypevar <|> parens pConsArgAtom <|> fmap D.Con pIdent)
      pure $ D.App name args

pToplevel :: Parser [Toplevel]
pToplevel =
  many $ parens (fmap TopFun pFun <|> fmap TopData pData)
