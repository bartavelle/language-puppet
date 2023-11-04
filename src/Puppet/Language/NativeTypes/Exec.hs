module Puppet.Language.NativeTypes.Exec (nativeExec) where

import qualified Data.Text as Text
import Puppet.Language.NativeTypes.Helpers

nativeExec :: (NativeTypeName, NativeTypeMethods)
nativeExec = ("exec", nativetypemethods parameterfunctions fullyQualifiedOrPath)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [ ("command", [nameval]),
    ("creates", [rarray, strings, fullyQualifieds]),
    ("cwd", [string, fullyQualified]),
    ("environment", [rarray, strings]),
    ("group", [string]),
    ("logoutput", [defaultvalue "false", string, values ["true", "false", "on_failure"]]),
    ("onlyif", [string]),
    ("path", [rarray, strings, fullyQualifieds]),
    ("provider", [string, values ["posix", "shell", "windows"]]),
    ("refresh", [string]),
    ("refreshonly", [defaultvalue "false", string, values ["true", "false"]]),
    ("returns", [defaultvalue "0", rarray, integers]),
    ("timeout", [defaultvalue "300", integer]),
    ("tries", [defaultvalue "1", integer]),
    ("try_sleep", [defaultvalue "0", integer]),
    ("unless", [string]),
    ("user", [string])
  ]

fullyQualifiedOrPath :: NativeTypeValidate
fullyQualifiedOrPath res =
  case (res ^. rattributes . at "path", res ^. rattributes . at "command") of
    (Nothing, Just (PString x)) ->
      if Text.head x == '/'
        then Right res
        else Left "Command must be fully qualified if path is not defined"
    _ -> Right res
