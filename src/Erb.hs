-- | Entry point to general Erb service
-- This module share no dependency with Puppet modules
module Erb (
    module Erb.Ruby
  , module Erb.Parser
) where

import           Erb.Parser
import           Erb.Ruby
