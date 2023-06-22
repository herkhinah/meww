{-# LANGUAGE ImportQualifiedPost #-}
module Raw.Parser (parse, pToplevel, pLet, pLetRec, pData, pLam) where

import Raw.Syntax


import Common (Name, Parser, Constant (..))
import Control.Monad
import Data.Char
import Data.Functor
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import qualified Raw.Syntax as Syntax
import Data.Void (Void)
import Type

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws


char :: Char -> Parser Char
char c = lexeme (C.char c)

parens :: Parser a -> Parser a
parens p = char '(' *> ws *> p <* ws <* char ')' <* ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  _ <- C.string kw <* ws
  pure ()


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

pConst :: Parser Raw
pConst = fmap RConst $ pStringLit <|> try pFloatLit <|> try pIntLit <|> pStringLit <|> try pBoolLit <|> pVoidLit


pAtomLast :: Parser Raw
pAtomLast = pAtom <|> try pConst <|> try pConst <|> try (fmap RVar pIdent) <* ws

pAtom :: Parser Raw
pAtom = parens (try pLet <|> try pLetRec <|> try pLam <|> try pCaseExpr <|> pApp) <* ws

pLet :: Parser Raw
pLet =  pKeyword "let" >> RLet <$> parens (fmap pure pDef <|> some (parens pDef)) <*> pAtomLast
  where
    pDef :: Parser (Name, Raw)
    pDef = (,) <$> pIdent <*> pAtomLast

pLetRec :: Parser Raw
pLetRec =  pKeyword "letrec" >> RLetRec <$> parens (fmap pure pDef <|> some (parens pDef)) <*> pAtomLast
  where
    pDef :: Parser (Name, Raw)
    pDef = (,) <$> pIdent <*> pAtomLast


pLam :: Parser Raw
pLam = pKeyword "lam" >> RLam <$> (parens (some pIdent) <|> fmap pure pIdent) <*> pAtomLast

pApp :: Parser Raw
pApp = RApp <$> pAtomLast <*> some pAtomLast

pBlock :: Parser Raw
pBlock = RBlock <$> some pAtomLast


pTypevar :: Parser Name
pTypevar = fmap pure (char '\'') <> pIdent <* ws



pCaseExpr :: Parser Raw
pCaseExpr = do
  pKeyword "case"
  expr <- pAtomLast
  cases <- some $ (,) <$> parens pPattern <*> pAtomLast
  pure $ RCase expr cases
  where
    pPattern :: Parser Pattern
    pPattern = PCons <$> pIdent <*> many (pBind <|> pHole)
      where
        pBind = PBind <$> pIdent
        pHole = pKeyword "_" >> pure PHole

pDef :: Parser Raw
pDef = do
  pKeyword "define"
  name <- RDefine <$> parens ((,,) <$> pIdent <*> some pIdent <*> some pAtomLast) 
  --body <- many pAtomLast
  undefined


pToplevel :: Parser [Toplevel]
pToplevel =
  many  (ws *> parens (fmap TopData pData <|> fmap TopFun pFun) <* ws)
  where
    pFun :: Parser Fun
    pFun = do
      pKeyword "fun"
      name <- pIdent
      args <- parens $ many pIdent
      body <- pAtomLast
      pure $ Syntax.Fun name args body
      
pData :: Parser Data
pData = do
  pKeyword "data"
  name <- pIdent
  args <- many pTypevar
  conss <- many (pCons <* ws)
  pure $ Data name args conss

pCons :: Parser Raw
pCons = do
  name <- pIdent
  args <- many $ parens pConsApp <|> ConsVar <$> pTypevar
  pure $ RCons name args

pConsApp :: Parser Ty
pConsApp = do
  nm <- pIdent
  args <- some $ parens pConsApp <|> ConsVar <$> pTypevar
  pure $ RCons nm args

parse :: String -> String -> Either (ParseErrorBundle String Void) [Toplevel]
parse = Text.Megaparsec.parse pToplevel