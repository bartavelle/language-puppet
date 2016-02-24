module Puppet.NativeTypes.Notify (nativeNotify) where

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes.Helpers

import qualified Data.Text                  as T

nativeNotify :: (NativeTypeName, NativeTypeMethods)
nativeNotify = ("notify", nativetypemethods parameterfunctions return)

parameterfunctions :: [(T.Text, [T.Text -> NativeTypeValidate])]
parameterfunctions =
    [("message"   , [])
    ,("withpath"  , [string, defaultvalue "false", values ["true","false"]])
    ]
