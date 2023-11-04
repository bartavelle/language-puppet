module Puppet.Language.NativeTypes.SshSecure
  ( nativeSshSecure,
  )
where

import Puppet.Language.NativeTypes.Helpers

nativeSshSecure :: (NativeTypeName, NativeTypeMethods)
nativeSshSecure = ("ssh_authorized_key_secure", nativetypemethods parameterfunctions (userOrTarget >=> keyIfPresent))

-- Autorequires: If Puppet is managing the user or user that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
  [ ("type", [string, defaultvalue "ssh-rsa", values ["rsa", "dsa", "ssh-rsa", "ssh-dss"]]),
    ("key", [string]),
    ("user", [string]),
    ("ensure", [defaultvalue "present", string, values ["present", "absent", "role"]]),
    ("target", [string]),
    ("options", [rarray, strings])
  ]

userOrTarget :: NativeTypeValidate
userOrTarget res =
  case (res ^. rattributes & has (ix "user"), res ^. rattributes & has (ix "target")) of
    (False, False) -> Left "Parameters user or target are mandatory"
    _ -> Right res

keyIfPresent :: NativeTypeValidate
keyIfPresent res =
  case (res ^. rattributes . at "key", res ^. rattributes . at "ensure") of
    (Just _, Just "present") -> Right res
    (_, Just "absent") -> Right res
    _ -> Left "Parameter key is mandatory when the resource is present"
