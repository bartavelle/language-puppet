module Puppet.NativeTypes.Exec (nativeExec) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Lens

nativeExec :: (PuppetTypeName, PuppetTypeMethods)
nativeExec = ("exec", PuppetTypeMethods validateExec parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset :: HS.HashSet T.Text
parameterset = HS.fromList $ map fst parameterfunctions

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
parameterfunctions =
    [("command"     , [nameval])
    ,("creates"     , [rarray, strings, fullyQualifieds])
    ,("cwd"         , [string, fullyQualified])
    ,("environment" , [rarray, strings])
    ,("group"       , [string])
    ,("logoutput"   , [defaultvalue "false", string, values ["true","false","on_failure"]])
    ,("onlyif"      , [string])
    ,("path"        , [rarray, strings, fullyQualifieds])
    ,("provider"    , [string, values ["posix","shell","windows"]])
    ,("refresh"     , [string])
    ,("refreshonly" , [defaultvalue "false", string, values ["true","false"]])
    ,("returns"     , [defaultvalue "0", rarray, integers])
    ,("timeout"     , [defaultvalue "300", integer])
    ,("tries"       , [defaultvalue "1", integer])
    ,("try_sleep"   , [defaultvalue "0", integer])
    ,("unless"      , [string])
    ,("user"        , [string])
    ]

validateExec :: PuppetTypeValidate
validateExec = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> fullyQualifiedOrPath

fullyQualifiedOrPath :: PuppetTypeValidate
fullyQualifiedOrPath res = case (res ^. rattributes . at "path", res ^. rattributes . at "command") of
                               (Nothing, Just (PString x)) -> if T.head x == '/'
                                                                       then Right res
                                                                       else Left "Command must be fully qualified if path is not defined"
                               _ -> Right res
