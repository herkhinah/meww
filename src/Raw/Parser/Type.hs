{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Raw.Parser.Type
(
    ty
)
where

import Control.Applicative
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Megaparsec ((<?>))

import Raw.Parser.Helper
import Raw.Syntax (PType(..), SyntaxInfo, typeArrowAllowed)
import Raw.Parser.Stack
import TT.Name (sUN)

bracketed :: SyntaxInfo -> MewwParser PType
bracketed syn = do
    fcStart <- extent $ lchar '('
    (tys, fc) <- withExtent $ P.sepBy (ty syn { typeArrowAllowed = False }) (keyword "->")
    fcEnd <- extent $ lchar ')'
    case reverse tys of
        [] -> return $ PUnit (fcStart <> fcEnd)
        [ty] -> return ty
        (ty : tys) -> return $ PArrow fc (reverse tys) ty


typeVar :: MewwParser PType
typeVar = (do
    (tv, fc) <- withExtent . token $ P.try $ (:) <$> char '\'' <*> many P.alphaNumChar
    return $ PTypevar fc (sUN tv))
    <?> "type variable"

ident :: MewwParser String
ident = token (P.try  $ (:) <$> P.upperChar <*> many P.alphaNumChar) <?> "type identifier"

ref :: SyntaxInfo -> MewwParser PType
ref syn = do
    (name, fc) <- withExtent (fmap sUN ident)
    args <- many $ P.try (ref syn) <|> typeVar <|> bracketed syn
    return $ PRef fc name args

ty :: SyntaxInfo -> MewwParser PType
ty syn = if typeArrowAllowed syn
    then do
        (tys, fc) <- withExtent (P.sepBy1 (P.try (ref syn) <|> typeVar <|> bracketed syn) (keyword "->")) <?> "type"
        case reverse tys of
            [ty] -> return ty
            (ty : tys) -> return $ PArrow fc (reverse tys) ty
    else P.try (ref syn) <|> typeVar <|> bracketed syn
    <?> "type"
