module Puppet.PP
    ( module Text.PrettyPrint.ANSI.Leijen
    , ttext
    , tshow
    , dq
    , pshow
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

