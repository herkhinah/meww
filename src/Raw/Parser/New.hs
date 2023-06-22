{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Raw.Parser.New where

import Control.Applicative hiding (some, many)
import Control.Monad (void, MonadPlus (mplus))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Type (Ty(..), Builtin (..), TyBuiltin (..))
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Raw.Syntax

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens p = do
    _ <- char '(' <* sc
    res <- p <* sc
    _ <- char ')' <* sc
    pure res

pIdent :: Parser String
pIdent = (:) <$> upperChar <*> many alphaNumChar <* sc


pExpr :: Parser Raw
pExpr = undefined
        

pType :: Parser Ty
pType = makeExprParser pAtom [ [InfixR ((string "->" <* sc) $> \l r -> case r of (TyArrow args ret) -> TyArrow (l : args) ret; _ -> TyArrow [l] r)] ]
    where
        pAtom :: Parser Ty
        pAtom = parens pType <|> pTypeVar <|> try pBuiltin <|> pSpine 

        pTypeVar :: Parser Ty
        pTypeVar = fmap TyVarS $ (:) <$> char '\'' <*> many alphaNumChar <* sc

        pSpine :: Parser Ty
        pSpine = TyData <$> pIdent <*> many pAtom

        pBuiltin :: Parser Ty
        pBuiltin = TyBuiltin <$>  (pList <|> pTuple <|> choice [ BBool <$ string "Bool", BInt <$ string "Int", BFloat <$ string "Float", BString <$ string "String"])
            where
                pList =  (char '[' *> sc *> (BList <$> pType) <* sc <* char ']' <*sc)
                pTuple = char '(' *> sc *> (BTuple <$> sepBy pType (char ',' *> sc)) <* sc <* char ')'



