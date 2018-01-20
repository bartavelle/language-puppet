-- | General puppet language specification.
--
-- This module doesn't depend on any other project modules (except for "XPrelude").
-- It serves as a common bridge that can be used in "PuppetDB" or "Facter" as well as in
-- "Puppet.Interpreter" or "Puppet.Parser".
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
