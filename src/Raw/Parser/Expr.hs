{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Raw.Parser.Expr where
import Raw.Parser.Helper
import Raw.Syntax

import Control.Applicative hiding (Const)
import qualified Control.Monad.Combinators.Expr as P
import Control.Monad.State.Strict
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import Raw.Parser.Stack
import TT.Name
import Raw.Parser.FC
import Raw.Parser.Ops (reservedOp)
import Raw.Parser.Type

caseExpr :: SyntaxInfo -> MewwParser PTerm
caseExpr syn = do keyword "case"
                  (scr, fc) <- withExtent $ expr syn
                  keyword "of"
                  opts <- indentedBlock1 (caseOption syn)
                  return (PCase fc scr opts)
               <?> "case expression"

caseOption :: SyntaxInfo -> MewwParser (PTerm, PTerm)
caseOption syn = do lhs <- expr (syn { inPattern = True })
                    r <- symbol "->" *> expr syn
                    return (lhs, r)
                 <?> "case option"


tyOptDeclList :: SyntaxInfo -> MewwParser [(Name, FC, PType)]
tyOptDeclList syn = P.sepBy1 (do (x, fc) <- withExtent nameOrPlaceholder
                                 t <- P.option PPlaceholder (do lchar ':'
                                                                ty (syn {typeArrowAllowed = False}))
                                 return (x, fc, t))
                                 (lchar ',')
                        <?> "type declaration list"
    where  nameOrPlaceholder :: MewwParser Name
           nameOrPlaceholder = name
                           <|> UB <$ lchar '_'
                           <?> "name or placeholder"


lambda :: SyntaxInfo -> MewwParser PTerm
lambda syn = do lchar '\\' <?> "lambda expression"
                (do xt <- P.try $ tyOptDeclList syn
                    (sc, fc) <- withExtent lambdaTail
                    return (bindList (PLam fc) xt sc))
                  <?> "lambda expression"
    where lambdaTail :: MewwParser PTerm
          lambdaTail = symbol "->" *> expr syn



constant :: Parsing m => m Const
constant = P.try (Fl <$> float)
       <|> BI <$> natural
       <|> Str <$> stringLiteral
       <|> P.try (Ch <$> charLiteral) --Currently ambigous with symbols
       <?> "constant or literal"

expr :: SyntaxInfo -> MewwParser PTerm
expr syn = P.makeExprParser (expr' syn) []

expr' :: SyntaxInfo -> MewwParser PTerm
expr' syn =
        lambda syn
    <|> block syn
    <|> caseExpr syn
    <|> app syn


bracketed :: SyntaxInfo -> MewwParser PTerm
bracketed syn = do (FC fn (sl, sc) _) <- extent (lchar '(') <?> "parenthesized expression"
                   bracketed' (FC fn (sl, sc) (sl, sc+1)) syn

bracketed' :: FC -> SyntaxInfo -> MewwParser PTerm
bracketed' open syn =
            do fc <- extent (addExtent open *> lchar ')')
               return $ PTrue fc
        <|> do l <- expr syn
               bracketedTy syn open l

bracketedTy :: SyntaxInfo -> FC -> PTerm -> MewwParser PTerm
bracketedTy syn openParenFC e =
             do lchar ')'; return e
        <|>  do exprs <- some (do comma <- extent (lchar ',')
                                  r <- expr syn
                                  return (r, comma))
                closeParenFC <- extent (lchar ')')
                return $ PPair (openParenFC <> closeParenFC) e (mergePairs exprs)
        <?> "end of bracketed expression"
  where mergePairs :: [(PTerm, FC)] -> PTerm
        mergePairs [(t, fc)]    = t
        mergePairs ((t, fc):rs) = PPair fc t (mergePairs rs)


simpleExpr :: SyntaxInfo -> MewwParser PTerm
simpleExpr syn =
            do (c, cfc) <- withExtent constant
               return (PConstant cfc c)
        <|> do (x, fc) <- withExtent name
               if inPattern syn
                  then P.option (PVar fc x)
                                (do reservedOp "@"
                                    (s, fcIn) <- withExtent $ simpleExpr syn
                                    return (PAs fcIn x s))
                  else return (PVar fc x)
        <|> listExpr syn
        <|> bracketed syn
        <?> "expression"


listExpr :: SyntaxInfo -> MewwParser PTerm
listExpr syn = do (FC f (l, c) _) <- extent (lchar '[')
                  (do (FC _ _ (l', c')) <- extent (lchar ']') <?> "end of list expression"
                      return (mkNil (FC f (l, c) (l', c'))))
                   <|> (do (x, fc) <- withExtent (expr syn) <?> "expression"
                           (do xs <- many (do commaFC <- extent (lchar ',') <?> "list element"
                                              elt <- expr syn
                                              return (elt, commaFC))
                               rbrackFC <- extent (lchar ']') <?> "end of list expression"
                               return (mkList fc rbrackFC ((x, FC f (l, c) (l, c+1)) : xs))))
                <?> "list expression"
  where
    mkNil :: FC -> PTerm
    mkNil fc = PVar fc (sUN "Nil")
    mkList :: FC -> FC -> [(PTerm, FC)] -> PTerm
    mkList errFC nilFC [] = PVar nilFC (sUN "Nil")
    mkList errFC nilFC ((x, fc) : xs) = PApp errFC (PVar fc (sUN "::")) [x, mkList errFC nilFC xs]

{-
listExpr :: SyntaxInfo -> MewwParser PTerm
listExpr syn = do (FC f (l, c) _) <- extent (lchar '[')
                  (do (FC _ _ (l', c')) <- extent (lchar ']') <?> "end of list expression"
                      return (mkNil (FC f (l, c) (l', c'))))
                   <|> (do (x, fc) <- withExtent (expr syn) <?> "expression"
                           (do xs <- many (do commaFC <- extent (lchar ',') <?> "list element"
                                              elt <- expr syn
                                              return (elt, commaFC))
                               rbrackFC <- extent (lchar ']') <?> "end of list expression"
                               return (mkList fc rbrackFC ((x, FC f (l, c) (l, c+1)) : xs))))
                <?> "list expression"
  where
    mkNil :: FC -> PTerm
    mkNil fc = PVar fc (sUN "Nil")
    mkList :: FC -> FC -> [(PTerm, FC)] -> PTerm
    mkList errFC nilFC [] = PVar nilFC (sUN "Nil")
    mkList errFC nilFC ((x, fc) : xs) = PApp errFC (PVar fc (sUN "::")) [x, mkList errFC nilFC xs]
-}
arg :: SyntaxInfo -> MewwParser PTerm
arg = simpleExpr


app :: SyntaxInfo -> MewwParser PTerm
app syn = appExtent (do
    f <- simpleExpr syn
    (do args <- many (do notEndApp; arg syn)
        case args of
          [] -> return $ const f
          _  -> return $ \fc -> PApp fc f args))
     <?> "function application"

block :: SyntaxInfo -> MewwParser PTerm
block syn
    = do keyword "do"
         PBlock <$> indentedBlock1 (block_ syn)
      <?> "do block"

block_ :: SyntaxInfo -> MewwParser PBlock
block_ syn =
       P.try (do (i, ifc) <- withExtent name
                 symbol "<-"
                 (e, fc) <- withExtent $ expr syn;
                 return (BlockBind fc i ifc e))
   <|> do (e, fc) <- withExtent $ expr syn
          return (BlockExp fc e)
   <?> "do block expression"

