{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Raw.Parser.Helper where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Functor (($>))
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P hiding (space)
import Raw.State (MewwState(..))
import Raw.Parser.Stack
import TT.Name(Name(..), sUN, sNS)
import Raw.Parser.FC
import Raw.Syntax (PTerm, PType)

type MewwParser = Parser MewwState

someSpace :: Parsing m => m ()
someSpace = many (simpleWhiteSpace <|> singleLineComment <|> multiLineComment) $> ()


token :: Parsing m => m a -> m a
token p = trackExtent p <* whiteSpace



-- | Parse a reserved identfier, highlighting it as a keyword
keyword :: Parsing m => String -> m ()
keyword = reserved


{- * Space, comments and literals (token/lexing like parsers) -}

-- | Consumes any simple whitespace (any character which satisfies Char.isSpace)
simpleWhiteSpace :: Parsing m => m ()
simpleWhiteSpace = void (P.satisfy isSpace)

-- | Checks if a charcter is end of line
isEol :: Char -> Bool
isEol '\n' = True
isEol  _   = False

-- | A parser that succeeds at the end of the line
eol :: Parsing m => m ()
eol = void (P.satisfy isEol) <|> P.lookAhead P.eof <?> "end of line"


{- | Consumes a single-line comment

@
     SingleLineComment_t ::= '--' ~EOL_t* EOL_t ;
@
 -}
singleLineComment :: Parsing m => m ()
singleLineComment = P.hidden (void (string "--") *> many (P.satisfy (not . isEol)) *> eol)

{- | Consumes a multi-line comment

@
  MultiLineComment_t ::=
     '{ -- }'
   | '{ -' InCommentChars_t
  ;
@

@
  InCommentChars_t ::=
   '- }'
   | MultiLineComment_t InCommentChars_t
   | ~'- }'+ InCommentChars_t
  ;
@
-}
multiLineComment :: Parsing m => m ()
multiLineComment = P.hidden $ P.try ((string "{-" *> string "-}") $> ())
                              <|> string "{-" *> inCommentChars
  where inCommentChars :: Parsing m => m ()
        inCommentChars =     (string "-}" $> ())
                         <|> P.try multiLineComment *> inCommentChars
                         <|> string "|||" *> many (P.satisfy (not . isEol)) *> eol *> inCommentChars
                         <|> P.skipSome (P.noneOf startEnd) *> inCommentChars
                         <|> P.oneOf startEnd *> inCommentChars
                         <?> "end of comment"
        startEnd :: String
        startEnd = "{}-"


-- | Parses some white space
whiteSpace :: Parsing m => m ()
whiteSpace = someSpace <|> pure ()

-- | Parses a string literal
stringLiteral :: Parsing m => m String
stringLiteral = token . P.try $ P.char '"' *> P.manyTill P.charLiteral (P.char '"')

-- | Parses a char literal
charLiteral :: Parsing m => m Char
charLiteral = token . P.try $ P.char '\'' *> P.charLiteral <* P.char '\''

-- | Parses a natural number
natural :: Parsing m => m Integer
natural = token (    P.try (P.char '0' *> P.char' 'x' *> P.hexadecimal)
                 <|> P.try (P.char '0' *> P.char' 'o' *> P.octal)
                 <|> P.try P.decimal)


-- | Parses a floating point number
float :: Parsing m => m Double
float = token . P.try $ P.float


char :: Parsing m => Char -> m Char
char = P.char

string :: Parsing m => String -> m String
string = P.string

reservedIdentifiers :: HS.HashSet String
reservedIdentifiers = HS.fromList
  [ "Type"
  , "case", "class", "codata", "constructor", "corecord", "data"
  , "do", "dsl", "else", "export", "if", "implementation", "implicit"
  , "import", "impossible", "in", "infix", "infixl", "infixr", "instance"
  , "interface", "let", "mutual", "namespace", "of", "parameters", "partial"
  , "postulate", "private", "proof", "public", "quoteGoal", "record"
  , "rewrite", "syntax", "then", "total", "using", "where", "with"
  ]

identifierOrReservedWithExtraChars :: Parsing m => String -> m String
identifierOrReservedWithExtraChars extraChars = token $ P.try $ do
  c <- P.satisfy isAlpha <|> P.oneOf "_"
  cs <- P.many (P.satisfy isAlphaNum <|> P.oneOf extraChars)
  return $ c : cs


-- | Parses a character as a token
lchar :: Parsing m => Char -> m Char
lchar = token . P.char

symbol :: Parsing m => String -> m ()
symbol = void . token . P.string


-- | Parses a reserved identifier
reserved :: Parsing m => String -> m ()
reserved name = token $ P.try $ do
  P.string name
  P.notFollowedBy (P.satisfy isAlphaNum <|> P.oneOf "_'.") <?> "end of " ++ name


-- | Parses an identifier as a token
identifierWithExtraChars :: Parsing m => String -> m String
identifierWithExtraChars extraChars = P.try $ do
  ident <- identifierOrReservedWithExtraChars extraChars
  when (ident `HS.member` reservedIdentifiers) $ P.unexpected . P.Label . NonEmpty.fromList $ "reserved " ++ ident
  when (ident == "_") $ P.unexpected . P.Label . NonEmpty.fromList $ "wildcard"
  return ident

identifier :: Parsing m => m String
identifier = identifierWithExtraChars "_'."

-- | Parses an identifier with possible namespace as a name
iName :: Parsing m => [String] -> m Name
iName bad = maybeWithNS identifier bad <?> "name"

-- | Parses an string possibly prefixed by a namespace
maybeWithNS :: Parsing m => m String -> [String] -> m Name
maybeWithNS parser bad = do
  i <- P.option "" (P.lookAhead identifier)
  when (i `elem` bad) $ P.unexpected . P.Label . NonEmpty.fromList $ "reserved identifier"
  mkName <$> P.choice (reverse (parserNoNS parser : parsersNS parser i))
  where parserNoNS :: Parsing m => m String -> m (String, String)
        parserNoNS = fmap (\x -> (x, ""))
        parserNS   :: Parsing m => m String -> String -> m (String, String)
        parserNS   parser ns = do xs <- trackExtent (string ns)
                                  lchar '.'
                                  x <- parser
                                  return (x, xs)
        parsersNS  :: Parsing m => m String -> String -> [m (String, String)]
        parsersNS parser i = [P.try (parserNS parser ns) | ns <- initsEndAt (=='.') i]

-- | Parses a name
name :: (Parsing m, MonadState MewwState m) => m Name
name = do
    keywords <- syntax_keywords <$> get
    aliases  <- module_aliases  <$> get
    n <- iName keywords
    return (unalias aliases n)
   <?> "name"
  where
    unalias :: M.Map [T.Text] [T.Text] -> Name -> Name
    unalias aliases (NS n ns) | Just ns' <- M.lookup ns aliases = NS n ns'
    unalias aliases name = name

{- | List of all initial segments in ascending order of a list.  Every
such initial segment ends right before an element satisfying the given
condition.
-}
initsEndAt :: (a -> Bool) -> [a] -> [[a]]
initsEndAt p [] = []
initsEndAt p (x:xs) | p x = [] : x_inits_xs
                    | otherwise = x_inits_xs
  where x_inits_xs = [x : cs | cs <- initsEndAt p xs]

{- | Create a `Name' from a pair of strings representing a base name and its
 namespace.
-}
mkName :: (String, String) -> Name
mkName (n, "") = sUN n
mkName (n, ns) = sNS (sUN n) (reverse (parseNS ns))
  where parseNS x = case span (/= '.') x of
                      (x, "")    -> [x]
                      (x, '.':y) -> x : parseNS y


{- * Position helpers -}

{-* Syntax helpers-}
-- | Bind constraints to term
bindList :: (Name -> FC -> PType -> PTerm -> PTerm) -> [(Name, FC, PType)] -> PTerm -> PTerm
bindList b []                 sc = sc
bindList b ((n, fc, t):bs) sc = b n fc t (bindList b bs sc)

{- | @commaSeparated p@ parses one or more occurences of `p`,
     separated by commas and optional whitespace. -}
commaSeparated :: Parsing m => m a -> m [a]
commaSeparated p = p `P.sepBy1` (P.space >> P.char ',' >> P.space)



-- | Push indentation to stack
pushIndent :: MewwParser ()
pushIndent = do columnNumber <- indent
                mst <- get
                put (mst { indent_stack = columnNumber : indent_stack mst })

-- | Pops indentation from stack
popIndent :: MewwParser ()
popIndent = do mst <- get
               case indent_stack mst of
                 [] -> error "The impossible happened! Tried to pop an indentation level where none was pushed (underflow)."
                 (x : xs) -> put (mst { indent_stack = xs })



-- | Gets current indentation
indent :: Parsing m => m Int
indent = P.unPos . P.sourceColumn <$> P.getSourcePos


-- | Checks if the following character matches provided parser
lookAheadMatches :: Parsing m => m a -> m Bool
lookAheadMatches p = isJust <$> P.lookAhead (P.optional p)


-- | Parses a start of block
openBlock :: MewwParser ()
openBlock =     do lchar '{'
                   mst <- get
                   put (mst { brace_stack = Nothing : brace_stack mst })
            <|> do mst <- get
                   lvl' <- indent
                    -- if we're not indented further, it's an empty block, so
                    -- increment lvl to ensure we get to the end
                   let lvl = case brace_stack mst of
                                   Just lvl_old : _ ->
                                       if lvl' <= lvl_old then lvl_old+1
                                                          else lvl'
                                   [] -> if lvl' == 1 then 2 else lvl'
                                   _ -> lvl'
                   put (mst { brace_stack = Just lvl : brace_stack mst })
            <?> "start of block"

-- | Parses an end of block
closeBlock :: MewwParser ()
closeBlock = do mst <- get
                bs <- case brace_stack mst of
                        []  -> [] <$ P.eof
                        Nothing : xs -> lchar '}' >> return xs <?> "end of block"
                        Just lvl : xs -> (do i   <- indent
                                             isParen <- lookAheadMatches (char ')')
                                             isIn <- lookAheadMatches (reserved "in")
                                             if i >= lvl && not (isParen || isIn)
                                                then fail "not end of block"
                                                else return xs)
                                          <|> (do notOpenBraces
                                                  P.eof
                                                  return [])
                put (mst { brace_stack = bs })

-- | Parses and keeps a terminator
keepTerminator :: MewwParser ()
keepTerminator =  void (lchar ';')
              <|> do c <- indent; l <- lastIndent
                     unless (c <= l) $ fail "not a terminator"
              <|> do isParen <- lookAheadMatches (P.oneOf ")}|")
                     isIn <- lookAheadMatches (reserved "in")
                     unless (isIn || isParen) $ fail "not a terminator"
              <|> P.lookAhead P.eof

-- | Gets last indentation
lastIndent :: (MonadState MewwState m) => m Int
lastIndent = do ist <- get
                case indent_stack ist of
                  (x : xs) -> return x
                  _        -> return 1


-- | Applies parser in an indented position
indented :: MewwParser a -> MewwParser a
indented p = notEndBlock *> p <* keepTerminator



-- | Applies parser to get a block with at least one statement (which has possibly indented statements)
indentedBlock1 :: MewwParser a -> MewwParser [a]
indentedBlock1 p = do openBlock
                      pushIndent
                      res <- some (indented p)
                      popIndent
                      closeBlock
                      return res

-- | Checks that there are no braces that are not closed
notOpenBraces :: MewwParser ()
notOpenBraces = do ist <- get
                   when (hasNothing $ brace_stack ist) $ fail "end of input"
  where hasNothing :: [Maybe a] -> Bool
        hasNothing = any isNothing

-- | Checks that it is not end of block
notEndBlock :: MewwParser ()
notEndBlock = do ist <- get
                 case brace_stack ist of
                      Just lvl : xs -> do i <- indent
                                          isParen <- lookAheadMatches (P.char ')')
                                          when (i < lvl || isParen) (fail "end of block")
                      _ -> return ()

-- | Checks if application expression does not end
notEndApp :: MewwParser ()
notEndApp = do c <- indent; l <- lastIndent
               when (c <= l) (fail "terminator")
