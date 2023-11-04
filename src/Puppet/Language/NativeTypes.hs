-- | This exposed module holds the /native/ Puppet resource types.
module Puppet.Language.NativeTypes
  ( baseNativeTypes,
    defaulttype,
    NativeTypeMethods,
    NativeTypeName,
    HasNativeTypeMethods (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Puppet.Language.NativeTypes.Concat (nativeConcat, nativeConcatFragment)
import Puppet.Language.NativeTypes.Cron (nativeCron)
import Puppet.Language.NativeTypes.Exec (nativeExec)
import Puppet.Language.NativeTypes.File (nativeFile)
import Puppet.Language.NativeTypes.Group (nativeGroup)
import Puppet.Language.NativeTypes.Helpers
  ( Container,
    HasNativeTypeMethods (..),
    NativeTypeMethods,
    NativeTypeName,
    defaulttype,
    faketype,
    map,
    (++),
  )
import Puppet.Language.NativeTypes.Host (nativeHost)
import Puppet.Language.NativeTypes.Mount (nativeMount)
import Puppet.Language.NativeTypes.Notify (nativeNotify)
import Puppet.Language.NativeTypes.Package (nativePackage)
import Puppet.Language.NativeTypes.SshSecure (nativeSshSecure)
import Puppet.Language.NativeTypes.User (nativeUser)
import Puppet.Language.NativeTypes.ZoneRecord (nativeZoneRecord)

fakeTypes :: [(NativeTypeName, NativeTypeMethods)]
fakeTypes = [faketype "class"]

defaultTypes :: [(NativeTypeName, NativeTypeMethods)]
defaultTypes = map defaulttype ["augeas", "computer", "filebucket", "interface", "k5login", "macauthorization", "mailalias", "maillist", "mcx", "nagios_command", "nagios_contact", "nagios_contactgroup", "nagios_host", "nagios_hostdependency", "nagios_hostescalation", "nagios_hostextinfo", "nagios_hostgroup", "nagios_service", "nagios_servicedependency", "nagios_serviceescalation", "nagios_serviceextinfo", "nagios_servicegroup", "nagios_timeperiod", "resources", "router", "schedule", "scheduledtask", "selboolean", "selmodule", "service", "ssh_authorized_key", "sshkey", "stage", "tidy", "vlan", "yumrepo", "zfs", "zone", "zpool"]

-- | The map of native types.
baseNativeTypes :: Container NativeTypeMethods
baseNativeTypes =
  HM.fromList
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
        : fakeTypes
        ++ defaultTypes
    )
