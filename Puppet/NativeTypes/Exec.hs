module Puppet.NativeTypes.Exec (nativeExec) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import qualified Data.Set as Set

nativeExec = ("exec", PuppetTypeMethods validateExec parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions = 
    [("command"     , [string, nameval])
    ,("creates"     , [string, fullyQualified])
    ,("cwd"         , [string, fullyQualified])
    ,("environment" , [rarray, strings])
    ,("group"       , [string])
    ,("logoutput"   , [defaultvalue "false", string, values ["true","false","on_failure"]])
    ,("onlyif"      , [string])
    ,("path"        , [rarray, strings, fullyQualifieds])
    ,("provider"    , [string, values ["posix","shell","windows"]])
    ,("refresh"     , [string])
    ,("refreshonly" , [defaultvalue "false", string, values ["true","false"]])
    ,("returns"     , [rarray, integers])
    ,("timeout"     , [integer])
    ,("tries"       , [integer])
    ,("try_sleep"   , [integer])
    ,("unless"      , [string])
    ,("user"        , [string])
    ]

validateExec :: PuppetTypeValidate
validateExec = defaultValidate parameterset >=> parameterFunctions parameterfunctions

