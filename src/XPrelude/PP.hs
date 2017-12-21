{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module XPrelude.PP (
  module Exports
  , PrettyError (..)
  , _PrettyError
  , displayNocolor
  , prettyToText
  , prettyToShow
  , ppline
  , pplines
  , pptext
  , ppstring
)
where

import           Protolude

import           Control.Lens
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

_PrettyError :: Prism' SomeException PrettyError
_PrettyError = prism' toException fromException

instance Monoid PrettyError where
  mempty = PrettyError mempty
  mappend a b = PrettyError $ getError a <+> getError b

instance IsString PrettyError where
  fromString = PrettyError . string

instance Exception PrettyError

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

prettyToText :: Doc -> Text
prettyToText = Text.pack . prettyToShow

prettyToShow :: Doc -> String
prettyToShow d = displayS (renderCompact d) ""

-- | A rendering function that drops colors.
displayNocolor :: Doc -> String
displayNocolor = flip displayS "" . dropEffects . renderPretty 0.4 180
  where
    dropEffects :: SimpleDoc -> SimpleDoc
    dropEffects (SSGR _ x)    = dropEffects x
    dropEffects (SLine l d)   = SLine l (dropEffects d)
    dropEffects (SText v t d) = SText v t (dropEffects d)
    dropEffects (SChar c d)   = SChar c (dropEffects d)
    dropEffects x             = x
