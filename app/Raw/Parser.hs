{-# OPTIONS -Wno-unused-imports #-}
module Raw.Parser where

import Raw.Syntax


import Common (Name, Parser, Constant (..))
import Control.Monad
import Data.Char
import Data.Functor
import Data.Functor.Identity qualified
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Raw.Syntax as Syntax

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme = L.lexeme ws

symbol s = lexeme (C.string s)

char c = lexeme (C.char c)

parens p = char '(' *> ws *> p <* ws <* char ')' <* ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  C.string kw <* ws
  pure ()

pList :: Parser a -> Parser [a]
pList p = parens (many p)

pIdent :: Parser String
pIdent = do
  ident <- takeWhile1P Nothing (\c -> not (isSpace c) && c /= '(' && c /= ')' ) <* ws
  guard ('\'' /= head ident
    && ident /= "_"
    && ident /= "lam"
    && ident /= "case"
    && ident /= "data"
    && ident /= "fun"
    && ident /= "let"
    && ident /= "letrec")
  pure ident

pVar = RVar <$> pIdent

pStringLit :: Parser Constant
pStringLit = fmap CString $ char '"' *> takeWhile1P Nothing (/= '"') <* char '"'

pFloatLit :: Parser Constant
pFloatLit = do
  str <-
    takeWhile1P Nothing isNumber
      <> C.string "."
      <> takeWhileP
        Nothing
        isNumber
  pure $ CFloat $ read str

pVoidLit :: Parser Constant
pVoidLit = C.string "()" $> CUnit

pIntLit :: Parser Constant
pIntLit = fmap (CInt . read) (takeWhile1P Nothing isNumber) <* ws

pBoolLit :: Parser Constant
pBoolLit = CBool <$> (fmap (const True) (pKeyword "true") <|> fmap (const False) (pKeyword "false"))

pConst = fmap RConst $ pStringLit <|> try pFloatLit <|> try pIntLit <|> pStringLit <|> try pBoolLit <|> pVoidLit


pAtom :: Parser Raw
pAtom =
  parens (try pLet <|> try pLam <|> try pCaseExpr <|> pApp)
  <|> try pConst
  <|> try (fmap RVar pIdent) <* ws

pLet :: Parser Raw
pLet =  pKeyword "let" >> RLet <$> parens (fmap pure pDef <|> some (parens pDef)) <*> pAtom
  where
    pDef :: Parser (Name, Raw)
    pDef = (,) <$> pIdent <*> pAtom

pLetRec :: Parser Raw
pLetRec =  pKeyword "letrec" >> RLetRec <$> parens (fmap pure pDef <|> some (parens pDef)) <*> pAtom
  where
    pDef :: Parser (Name, Raw)
    pDef = (,) <$> pIdent <*> pAtom


pLam :: Parser Raw
pLam = pKeyword "lam" >> RLam <$> (parens (some pIdent) <|> fmap pure pIdent) <*> pAtom

pPrimOp :: Parser PrimOp
pPrimOp =
  ( C.string "+" $> Plus
      <|> C.string "-" $> Minus
      <|> C.string "*" $> Mult
      <|> C.string "print" $> Print
  )
    <* ws

pApp :: Parser Raw
pApp = RApp <$> pAtom <*> some pAtom


pTypevar :: Parser Name
pTypevar = fmap pure (char '\'') <> pIdent <* ws



pCaseExpr :: Parser Raw
pCaseExpr = do
  pKeyword "case"
  expr <- pAtom
  cases <- some $ (,) <$> parens pPattern <*> pAtom
  pure $ RCase expr cases
  where
    pPattern :: Parser Pattern
    pPattern = PCons <$> pIdent <*> many (pBind <|> pHole)
      where
        pBind = PBind <$> pIdent
        pHole = pKeyword "_" >> pure PHole




pToplevel :: Parser [Toplevel]
pToplevel =
  many  (ws *> parens (fmap TopData pData <|> fmap TopFun pFun) <* ws)
  where
    pFun :: Parser Fun
    pFun = do
      pKeyword "fun"
      name <- pIdent
      args <- parens $ many pIdent
      body <- pAtom
      pure Syntax.Fun {name = name, args = args, body = body}

pData :: Parser Data
pData = do
  pKeyword "data"
  name <- pIdent
  args <- parens $ many pTypevar
  cons <- some $ parens pCons <* ws
  pure Data {name = name, args = args, cons = cons}
pCons :: Parser Cons
pCons = do
  name <- pIdent
  args <- many $ parens pConsApp <|> ConsVar <$> pTypevar
  pure $ Cons name args

pConsApp :: Parser Cons
pConsApp = do
  nm <- pIdent
  args <- some $ parens pConsApp <|> ConsVar <$> pTypevar
  pure $ Cons nm args


parse = Text.Megaparsec.parse pToplevel