module Puppet.Parser.Utils where

import           Text.Megaparsec.Pos

import           Puppet.Parser.Types


-- pretty  gives me a Doc
dummyppos :: PPosition
dummyppos = initialPPos "dummy"

dummypos :: Position
dummypos = initialPos "dummy"
