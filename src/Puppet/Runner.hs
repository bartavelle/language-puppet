-- | At the top of the abstraction level, the module exposes all high-end services:
--
-- * the preferences container
-- * the puppet daemon
-- * the statistic module
-- * the stdlib functions
-- * a bunch of pure runners
--
-- Naturally nothing from "Runner" should be used in lower abstraction layers.
module Puppet.Runner(
    module Puppet.Runner.Daemon
  , module Puppet.Runner.Preferences
  , module Puppet.Runner.Pure
  , module Puppet.Runner.Stats
  , module Puppet.Runner.Stdlib
  , module Puppet.Language
  )
  where

import Puppet.Runner.Daemon
import Puppet.Runner.Preferences
import Puppet.Runner.Stats
import Puppet.Runner.Stdlib
import Puppet.Runner.Pure
import Puppet.Language
