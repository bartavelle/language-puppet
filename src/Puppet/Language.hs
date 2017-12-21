-- | General puppet language specification
-- This module can be safely exported from all other Puppet module layers
-- such as Puppet.Parser or Puppet.Interpreter
module Puppet.Language(
    module Puppet.Language.Core
  , module Puppet.Language.NativeTypes
  , module Puppet.Language.Paths
  , module Puppet.Language.Resource
  , module Puppet.Language.Value
  , module Puppet.Language.WireCatalog
)
where

import Puppet.Language.Core
import Puppet.Language.Resource
import Puppet.Language.Value
import Puppet.Language.WireCatalog
import Puppet.Language.NativeTypes
import Puppet.Language.Paths
