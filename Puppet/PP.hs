module Puppet.PP
    ( module Text.PrettyPrint.ANSI.Leijen
    , ttext
    , tshow
    , dq
    , pshow
    , displayNocolor
    ) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import qualified Data.Text as T

ttext :: T.Text -> Doc
ttext = text . T.unpack

tshow :: Show a => a -> T.Text
tshow = T.pack . show

dq :: T.Text -> T.Text
dq x = T.cons '"' (T.snoc x '"')

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


