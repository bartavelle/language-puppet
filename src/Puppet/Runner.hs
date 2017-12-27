-- | At the top of the abstraction level, the module exposes all high-end services:
--
-- * the preferences container
-- * the puppet daemon
-- * the statistic module
-- * the stdlib functions
-- * a bunch of pure runners
--
-- Naturally nothing from "Puppet.Runner" should be used in lower abstraction layers.
module Puppet.Runner(
  -- * Preferences
    module Puppet.Runner.Preferences
  -- * Pure
  , module Puppet.Runner.Pure
  -- * Stats
  , module Puppet.Runner.Stats
  , module Puppet.Runner.Stdlib
  , module Puppet.Runner.Daemon
  , module Puppet.Interpreter
  )
  where

import Puppet.Runner.Daemon
import Puppet.Runner.Preferences
import Puppet.Runner.Stats
import Puppet.Runner.Stdlib
import Puppet.Runner.Pure
import Puppet.Interpreter
