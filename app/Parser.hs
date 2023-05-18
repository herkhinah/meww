{-# OPTIONS -Wno-unused-imports #-}
module Parser where

import Common (Name, Parser)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Functor.Identity qualified
import Data.Void
import Syntax
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

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

pStringLit :: Parser Literal
pStringLit = fmap LitString $ char '"' *> takeWhile1P Nothing (/= '"') <* char '"'

pFloatLit :: Parser Literal
pFloatLit = do
  str <-
    takeWhile1P Nothing isNumber
      <> C.string "."
      <> takeWhileP
        Nothing
        isNumber
  pure $ LitFloat $ read str

pVoidLit :: Parser Literal
pVoidLit = C.string "()" $> LitVoid

pIntLit :: Parser Literal
pIntLit = fmap (LitInt . read) (takeWhile1P Nothing isNumber) <* ws

pLit = pStringLit <|> try pFloatLit <|> pIntLit <|> pStringLit <|> pVoidLit

pAtom :: Parser Raw
pAtom =
  parens (try pLet <|> try pLam <|> pApp)
  <|> try (fmap RVar pIdent)
  <|> try (fmap RLiteral pLit) <* ws

pLet :: Parser Raw
pLet = do
  pKeyword "let"
  defs <- parens $ fmap pure pDef <|> some (parens pDef)
  tm <- pAtom
  pure $ foldr (\(nm, def) tm -> RLet nm def tm) tm defs
  where
    pDef :: Parser (Name, Raw)
    pDef = do
      ident <- pIdent
      tm <- pAtom
      pure (ident, tm)

pLam :: Parser Raw
pLam = do
  pKeyword "lam"
  args <- parens (some pIdent) <|> fmap pure pIdent
  body <- pAtom
  pure $ foldr RLam body args

pPrimOp :: Parser PrimOp
pPrimOp =
  ( C.string "+" $> Plus
      <|> C.string "-" $> Minus
      <|> C.string "*" $> Mult
      <|> C.string "print" $> Print
  )
    <* ws

pApp :: Parser Raw
pApp = do
  rator <- pAtom
  rand <- some pAtom
  pure $ foldl RApp rator rand

pFun :: Parser Fun
pFun = do
  pKeyword "fun"
  name <- pIdent
  args <- pList pIdent
  body <- pAtom
  pure Syntax.Fun {name = name, args = args, body = body}

pTypevar :: Parser Name
pTypevar = fmap pure (char '\'') <> pIdent

pData :: Parser Data
pData = do
  pKeyword "data"
  name <- pIdent
  args <- fmap concat (optional $ pList pTypevar)
  cons <- many (parens pCons)
  pure Data {name = name, args = args, cons = cons}

pCons :: Parser Cons
pCons = do
  name <- pIdent
  args <- many pConsArg
  pure Cons {name = name, args = args}

pConsArg :: Parser ConsArg
pConsArg = fmap TypeVar pTypevar <|> fmap ConsName pIdent <|> parens (fmap ConsApp pIdent <*> some pConsArg)

pConsArgAtom :: Parser ConsArg
pConsArgAtom = try (fmap TypeVar pTypevar) <|> try (fmap ConsName pIdent) <|> pApplied
  where
    pApplied :: Parser ConsArg
    pApplied = parens $ do
      name <- pIdent
      args <- many (fmap TypeVar pTypevar <|> parens pConsArgAtom <|> fmap ConsName pIdent)
      pure $ ConsApp name args

pToplevel :: Parser [Toplevel]
pToplevel =
  many $ parens (fmap TopFun pFun <|> fmap TopData pData)

parse = Text.Megaparsec.parse pToplevel ""