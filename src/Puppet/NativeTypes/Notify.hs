module Puppet.NativeTypes.Notify (nativeNotify) where

import           Puppet.Prelude

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes.Helpers

nativeNotify :: (NativeTypeName, NativeTypeMethods)
nativeNotify = ("notify", nativetypemethods parameterfunctions return)

parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
    [("message"   , [])
    ,("withpath"  , [string, defaultvalue "false", values ["true","false"]])
    ]
