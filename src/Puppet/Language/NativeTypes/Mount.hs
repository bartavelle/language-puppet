module Puppet.Language.NativeTypes.Mount (nativeMount) where

import Puppet.Language.NativeTypes.Helpers

nativeMount :: (NativeTypeName, NativeTypeMethods)
nativeMount = ("mount", nativetypemethods parameterfunctions return)

parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [ ("atboot", [string, values ["true", "false"]]),
    ("blockdevice", [string]),
    ("device", [string, mandatoryIfNotAbsent]),
    ("dump", [integer, inrange 0 2]),
    ("ensure", [defaultvalue "present", string, values ["present", "absent", "mounted"]]),
    ("fstype", [string, mandatoryIfNotAbsent]),
    ("name", [nameval]),
    ("options", [string]),
    ("pass", [defaultvalue "0", integer]),
    ("provider", [defaultvalue "parsed", string, values ["parsed"]]),
    ("remounts", [string, values ["true", "false"]]),
    ("target", [string, fullyQualified])
  ]
