{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Parsec
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, undecidable instances
--
-- This module provides instances that permit @parsec@ parsers to use
-- the combinators from the @parsers@ library.
----------------------------------------------------------------------------
module Text.Parser.Parsec () where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Text.Parser.Char as P
import qualified Text.Parser.Combinators as P
import qualified Text.Parser.LookAhead as P
import qualified Text.Parser.Token as P
import Data.Char
import Control.Monad

instance (Stream s m t, Show t) => P.Parsing (ParsecT s u m) where
  try           = try
  (<?>)         = (<?>)
  skipMany      = skipMany
  skipSome      = skipMany1
  unexpected    = unexpected
  eof           = eof
  notFollowedBy = notFollowedBy

instance (Stream s m t, Show t) => P.LookAheadParsing (ParsecT s u m) where
  lookAhead = lookAhead

instance Stream s m Char => P.CharParsing (ParsecT s u m) where
  satisfy = satisfy
  char    = char
  notChar c = satisfy (/= c)
  anyChar = anyChar
  string  = string

instance Stream s m Char => P.TokenParsing (ParsecT s u m)  where
  someSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment)
    where
        simpleSpace = P.skipSome (satisfy isSpace)
        oneLineComment = char '#' >> void (manyTill anyChar newline)
        multiLineComment = try (string "/*") >> inComment
        inComment =     void (try (string "*/"))
                    <|> (P.skipSome (noneOf "*/") >> inComment)
                    <|> (oneOf "*/" >> inComment)

