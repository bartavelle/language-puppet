{-| This module holds the /native/ Puppet resource types. -}
module Puppet.NativeTypes (
    baseNativeTypes
  , validateNativeType
) where

import           Control.Lens
import           Control.Monad.Operational
import qualified Data.HashMap.Strict as HM

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes.Concat
import           Puppet.NativeTypes.Cron
import           Puppet.NativeTypes.Exec
import           Puppet.NativeTypes.File
import           Puppet.NativeTypes.Group
import           Puppet.NativeTypes.Helpers
import           Puppet.NativeTypes.Host
import           Puppet.NativeTypes.Mount
import           Puppet.NativeTypes.Package
import           Puppet.NativeTypes.SshSecure
import           Puppet.NativeTypes.User
import           Puppet.NativeTypes.ZoneRecord

fakeTypes :: [(PuppetTypeName, PuppetTypeMethods)]
fakeTypes = map faketype ["class"]

defaultTypes :: [(PuppetTypeName, PuppetTypeMethods)]
defaultTypes = map defaulttype ["augeas","computer","filebucket","interface","k5login","macauthorization","mailalias","maillist","mcx","nagios_command","nagios_contact","nagios_contactgroup","nagios_host","nagios_hostdependency","nagios_hostescalation","nagios_hostextinfo","nagios_hostgroup","nagios_service","nagios_servicedependency","nagios_serviceescalation","nagios_serviceextinfo","nagios_servicegroup","nagios_timeperiod","notify","package","resources","router","schedule","scheduledtask","selboolean","selmodule","service","ssh_authorized_key","sshkey","stage","tidy","vlan","yumrepo","zfs","zone","zpool"]

-- | The map of native types. They are described in "Puppet.NativeTypes.Helpers".
baseNativeTypes :: Container PuppetTypeMethods
baseNativeTypes = HM.fromList
    ( nativeHost
    : nativeMount
    : nativeGroup
    : nativeFile
    : nativeZoneRecord
    : nativeCron
    : nativeExec
    : nativePackage
    : nativeUser
    : nativeSshSecure
    : fakeTypes ++ concatTypes ++ defaultTypes)

-- | Contrary to the previous iteration, this will let non native types pass.
validateNativeType :: Resource -> InterpreterMonad Resource
validateNativeType r = do
    tps <- singleton GetNativeTypes
    case tps ^. at (r ^. rid . itype) of
        Just x -> case (x ^. puppetValidate) r of
                      Right nr -> return nr
                      Left err -> throwPosError ("Invalid resource" <+> pretty r </> getError err)
        Nothing -> return r
