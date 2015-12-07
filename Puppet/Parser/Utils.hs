module Puppet.Parser.Utils where

import           Text.Megaparsec.Pos

import           Puppet.Parser.Types

dummyppos :: PPosition
dummyppos = initialPPos "dummy"

dummypos :: Position
dummypos = initialPos "dummy"
