{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Raw.Parser.Stack where
  
import Control.Arrow (app)
import Control.Monad.State.Strict (StateT(..), evalStateT)
import Control.Monad.Writer.Strict (MonadWriter(..), WriterT(..))
import qualified Control.Monad.Fail as Fail
import Data.Void (Void)
import qualified Text.Megaparsec as P
import Raw.Parser.FC
import System.FilePath (splitFileName, addTrailingPathSeparator)

type Parsing m = (Fail.MonadFail m, P.MonadParsec Void String m, MonadWriter FC m)


type Parser s = StateT s (WriterT FC (P.Parsec Void String))

-- | Run the Idris parser stack
runparser :: Parser st res -> st -> String -> String -> Either ParseError res
runparser p i inputname s =
  case P.parse (runWriterT (evalStateT p i)) inputname s of
    Left err -> Left $ ParseError err
    Right v  -> Right $ fst v

{- * Parse errors -}

newtype ParseError = ParseError { unParseError :: P.ParseErrorBundle String Void } deriving (Show)



{- * Tracking the extent of productions -}

sourcePositionFC :: P.SourcePos -> FC
sourcePositionFC (P.SourcePos name line column) =
  FC f (lineNumber, columnNumber) (lineNumber, columnNumber)
  where
    lineNumber = P.unPos line
    columnNumber = P.unPos column
    (dir, file) = splitFileName name
    f = if dir == addTrailingPathSeparator "."
        then file
        else name


-- | Get the current parse position.
--
-- This is useful when the position is needed in a way unrelated to the
-- heirarchy of parsers.  Prefer using @withExtent@ and friends.
getFC :: Parsing m => m FC
getFC = sourcePositionFC <$> P.getSourcePos

-- | Add an extent (widen) our current parsing context.
addExtent :: MonadWriter FC m => FC -> m ()
addExtent = tell


-- | Run a parser and track its extent.
--
-- Wrap bare Megaparsec parsers with this to make them "visible" in error
-- messages.  Do not wrap whitespace or comment parsers.  If you find an
-- extent is taking trailing whitespace, it's likely there's a double-wrapped
-- parser (usually via @Idris.Parser.Helpers.token@).
trackExtent :: Parsing m => m a -> m a
trackExtent p = do ~(FC f (sr, sc) _) <- getFC
                   result <- p
                   ~(FC f _ (er, ec)) <- getFC
                   addExtent (FC f (sr, sc) (er, max 1 (ec - 1)))
                   return result

-- | Run a parser, discard its value, and return its extent.
extent :: MonadWriter FC m => m a -> m FC
extent = fmap snd . withExtent

-- | Run a parser and return its value and extent.
withExtent :: MonadWriter FC m => m a -> m (a, FC)
withExtent = listen

-- | Run a parser and inject the extent after via function application.
appExtent :: MonadWriter FC m => m (FC -> a) -> m a
appExtent = fmap app . withExtent
