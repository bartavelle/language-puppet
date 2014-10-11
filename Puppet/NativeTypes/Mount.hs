module Puppet.NativeTypes.Mount (nativeMount) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import qualified Data.Text as T

nativeMount :: (NativeTypeName, NativeTypeMethods)
nativeMount = ("mount", ptypemethods parameterfunctions return)

parameterfunctions :: [(T.Text, [T.Text -> NativeTypeValidate])]
parameterfunctions =
    [("atboot"      , [string, values ["true","false"]])
    ,("blockdevice" , [string])
    ,("device"      , [string, mandatory])
    ,("dump"        , [integer, inrange 0 2])
    ,("ensure"      , [defaultvalue "present", string, values ["present","absent","mounted"]])
    ,("fstype"      , [string, mandatory])
    ,("name"        , [nameval])
    ,("options"     , [string])
    ,("pass"        , [defaultvalue "0", integer])
    ,("provider"    , [defaultvalue "parsed", string, values ["parsed"]])
    ,("remounts"    , [string, values ["true","false"]])
    ,("target"      , [string, fullyQualified])
    ]
