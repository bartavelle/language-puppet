module Puppet.Language.NativeTypes.Notify (nativeNotify) where

import           Puppet.Language.NativeTypes.Helpers

nativeNotify :: (NativeTypeName, NativeTypeMethods)
nativeNotify = ("notify", nativetypemethods parameterfunctions return)

parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [("message"   , [])
  ,("withpath"  , [string, defaultvalue "false", values ["true","false"]])
  ]
