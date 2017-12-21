-- | This exposed module holds the /native/ Puppet resource types.
module Puppet.Language.NativeTypes (
    baseNativeTypes
  , defaulttype
  , NativeTypeMethods
  , NativeTypeName
  , HasNativeTypeMethods(..)
) where

import qualified Data.HashMap.Strict                       as HM

import           Puppet.Language.NativeTypes.Concat
import           Puppet.Language.NativeTypes.Cron
import           Puppet.Language.NativeTypes.Exec
import           Puppet.Language.NativeTypes.File
import           Puppet.Language.NativeTypes.Group
import           Puppet.Language.NativeTypes.Helpers
import           Puppet.Language.NativeTypes.Host
import           Puppet.Language.NativeTypes.Mount
import           Puppet.Language.NativeTypes.Notify
import           Puppet.Language.NativeTypes.Package
import           Puppet.Language.NativeTypes.SshSecure
import           Puppet.Language.NativeTypes.User
import           Puppet.Language.NativeTypes.ZoneRecord

fakeTypes :: [(NativeTypeName, NativeTypeMethods)]
fakeTypes = map faketype ["class"]

defaultTypes :: [(NativeTypeName, NativeTypeMethods)]
defaultTypes = map defaulttype ["augeas","computer","filebucket","interface","k5login","macauthorization","mailalias","maillist","mcx","nagios_command","nagios_contact","nagios_contactgroup","nagios_host","nagios_hostdependency","nagios_hostescalation","nagios_hostextinfo","nagios_hostgroup","nagios_service","nagios_servicedependency","nagios_serviceescalation","nagios_serviceextinfo","nagios_servicegroup","nagios_timeperiod","resources","router","schedule","scheduledtask","selboolean","selmodule","service","ssh_authorized_key","sshkey","stage","tidy","vlan","yumrepo","zfs","zone","zpool"]

-- | The map of native types.
baseNativeTypes :: Container NativeTypeMethods
baseNativeTypes = HM.fromList
    ( nativeConcat
    : nativeConcatFragment
    : nativeCron
    : nativeExec
    : nativeFile
    : nativeGroup
    : nativeHost
    : nativeMount
    : nativeNotify
    : nativePackage
    : nativeSshSecure
    : nativeUser
    : nativeZoneRecord
    : fakeTypes ++ defaultTypes)
