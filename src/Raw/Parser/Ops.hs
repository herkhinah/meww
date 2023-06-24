{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Raw.Parser.Ops where

import Raw.Parser.Helper (token, string)

import qualified Text.Megaparsec as P
import Text.Megaparsec ((<?>))


import Raw.Parser.Stack (Parsing)


opChars :: String
opChars = ":!#$%&*+./<=>?@\\^|-~"

operatorLetter :: Parsing m => m Char
operatorLetter = P.oneOf opChars

-- Taken from Parsec (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- | Parses a reserved operator
reservedOp :: Parsing m => String -> m ()
reservedOp name = token $ P.try $
  do string name
     P.notFollowedBy operatorLetter <?> ("end of " ++ show name)
