{-| This module holds the /native/ Puppet resource types. -}
module Puppet.NativeTypes (baseNativeTypes) where

import Puppet.NativeTypes.Helpers
import Puppet.NativeTypes.File
import Puppet.NativeTypes.Cron
import Puppet.NativeTypes.Exec
import Puppet.NativeTypes.Group
import Puppet.NativeTypes.Host
import Puppet.NativeTypes.Mount
import Puppet.NativeTypes.ZoneRecord
import Puppet.Interpreter.Types
import qualified Data.Map as Map

fakeTypes = map faketype ["class", "ssh_authorized_key_secure"]

defaultTypes = map defaulttype ["augeas","computer","filebucket","interface","k5login","macauthorization","mailalias","maillist","mcx","nagios_command","nagios_contact","nagios_contactgroup","nagios_host","nagios_hostdependency","nagios_hostescalation","nagios_hostextinfo","nagios_hostgroup","nagios_service","nagios_servicedependency","nagios_serviceescalation","nagios_serviceextinfo","nagios_servicegroup","nagios_timeperiod","notify","package","resources","router","schedule","scheduledtask","selboolean","selmodule","service","sshauthorizedkey","sshkey","stage","tidy","user","vlan","yumrepo","zfs","zone","zpool"]

-- | The map of native types. They are described in "Puppet.NativeTypes.Helpers".
baseNativeTypes :: Map.Map PuppetTypeName PuppetTypeMethods
baseNativeTypes = Map.fromList
    ( nativeHost
    : nativeMount
    : nativeGroup
    : nativeFile
    : nativeZoneRecord
    : nativeCron
    : nativeExec
    : fakeTypes ++ defaultTypes)
