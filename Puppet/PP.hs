module Puppet.PP
    ( ttext
    , pshow
    , displayNocolor
      -- * Re-exports
    , module Text.PrettyPrint.ANSI.Leijen
    ) where

import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

ttext :: T.Text -> Doc
ttext = text . T.unpack

pshow :: Doc -> String
pshow d = displayS (renderPretty 0.4 120 d) ""

-- | A rendering function that drops colors:
displayNocolor :: Doc -> String
displayNocolor = flip displayS "" . dropEffects . renderPretty 0.4 180
    where
        dropEffects :: SimpleDoc -> SimpleDoc
        dropEffects (SSGR _ x) = dropEffects x
        dropEffects (SLine l d) = SLine l (dropEffects d)
        dropEffects (SText v t d) = SText v t (dropEffects d)
        dropEffects (SChar c d) = SChar c (dropEffects d)
        dropEffects x = x
