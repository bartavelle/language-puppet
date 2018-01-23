{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module XPrelude.PP (
  module Exports
  , PrettyError (..)
  , prettyToShow
  , ppline
  , pplines
  , pptext
  , ppstring
)
where

import           Protolude

import           Data.Scientific
import           Data.String
import qualified Data.Text                    as Text
import           Text.PrettyPrint.ANSI.Leijen as Exports hiding (bool, cat,
                                                          char, double, empty,
                                                          float, group, int,
                                                          integer, rational,
                                                          string, text, (<$>))

import           Text.PrettyPrint.ANSI.Leijen (string, text)

newtype PrettyError = PrettyError
  { getError :: Doc
  } deriving Show

instance Monoid PrettyError where
  mempty = PrettyError mempty
  mappend a b = PrettyError $ align (vsep [getError a, getError b])

instance IsString PrettyError where
  fromString = PrettyError . string

instance Pretty PrettyError where
  pretty = getError

instance Pretty Scientific where
  pretty = text . show


-- | pretty print multiple lines of text.
pplines :: Text -> Doc
pplines = string . Text.unpack

-- | pretty print a single line of text.
ppline :: Text -> Doc
ppline = text . Text.unpack

-- | pretty print multiple lines of string.
ppstring :: String -> Doc
ppstring = string

-- | pretty print one line of string
pptext :: String -> Doc
pptext = text

prettyToShow :: Doc -> String
prettyToShow d = displayS (renderCompact d) ""
