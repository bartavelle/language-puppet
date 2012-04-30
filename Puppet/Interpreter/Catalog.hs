module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Preferences
import Puppet.Interpreter.Types
import System.IO

getCatalog :: String -> Facts -> Catalog
getCatalog nodename facts = []

