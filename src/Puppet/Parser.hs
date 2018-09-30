{-| Parse puppet source code from text. -}
module Puppet.Parser (
  -- * Runner
    runPuppetParser
  -- * Parsers
  , puppetParser
  , prettyParseError
  -- * Pretty Print
  , module Puppet.Parser.PrettyPrinter
  , module Puppet.Parser.Types
  , module Puppet.Parser.Lens
) where

import           XPrelude

import           Text.Megaparsec

import           Puppet.Parser.PrettyPrinter
import           Puppet.Parser.Lens
import           Puppet.Parser.Internal
import           Puppet.Parser.Types


-- | Build a 'PrettyError' from a 'ParseError' given the text source.
-- The source is used to display the line on which the error occurs.
prettyParseError :: ParseErrorBundle Text Void -> PrettyError
prettyParseError err = PrettyError $ "cannot parse" <+> pretty (errorBundlePretty err)

-- | Run a puppet parser against some 'Text' input.
runPuppetParser :: String -> Text -> Either (ParseErrorBundle Text Void) (Vector Statement)
runPuppetParser = parse puppetParser

-- | Parse a collection of puppet 'Statement'.
puppetParser :: Parser (Vector Statement)
puppetParser = optional sc >> statementList
