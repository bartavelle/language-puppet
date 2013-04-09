module Puppet.NativeTypes.SshSecure (nativeSshSecure) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.Set as Set
import qualified Data.Map as Map

nativeSshSecure :: (PuppetTypeName, PuppetTypeMethods)
nativeSshSecure = ("ssh_authorized_key_secure", PuppetTypeMethods validateSshSecure parameterset)

-- Autorequires: If Puppet is managing the user or user that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions =
    [("type"    , [string, defaultvalue "ssh-rsa", values ["rsa","dsa","ssh-rsa","ssh-dss"]])
    ,("key"     , [string])
    ,("user"    , [string])
    ,("ensure"  , [defaultvalue "present", string, values ["present","absent","role"]])
    ,("target"  , [string])
    ,("options" , [rarray, strings])
    ]

userOrTarget :: PuppetTypeValidate
userOrTarget res = case (Map.lookup "user" (rrparams res), Map.lookup "target" (rrparams res)) of
                       (Nothing, Nothing) -> Left "Parameters user or target are mandatory"
                       _                  -> Right res


validateSshSecure :: PuppetTypeValidate
validateSshSecure = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> userOrTarget

