module Puppet.PP
    ( ttext
    , prettyToText
    , displayNocolor
      -- * Re-exports
    , module Text.PrettyPrint.ANSI.Leijen
    ) where

import Puppet.Prelude

import qualified Data.Text as Text
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

ttext :: Text -> Doc
ttext = text . Text.unpack

prettyToText :: Doc -> Text
prettyToText = Text.pack . prettyToShow

prettyToShow :: Doc -> String
prettyToShow d = displayS (renderCompact d) ""

-- | A rendering function that drops colors.
displayNocolor :: Doc -> String
displayNocolor = flip displayS "" . dropEffects . renderPretty 0.4 180
    where
        dropEffects :: SimpleDoc -> SimpleDoc
        dropEffects (SSGR _ x) = dropEffects x
        dropEffects (SLine l d) = SLine l (dropEffects d)
        dropEffects (SText v t d) = SText v t (dropEffects d)
        dropEffects (SChar c d) = SChar c (dropEffects d)
        dropEffects x = x
