module Puppet.Language.NativeTypes.Group (nativeGroup) where

import           Puppet.Language.NativeTypes.Helpers

nativeGroup :: (NativeTypeName, NativeTypeMethods)
nativeGroup = ("group", nativetypemethods parameterfunctions return)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
    [("allowdupe"               , [string, defaultvalue "false", values ["true","false"]])
    ,("attribute_membership"    , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("attributes"              , [strings])
    ,("auth_membership"         , [defaultvalue "minimum", string, values ["inclusive","minimum"]])
    ,("ensure"                  , [defaultvalue "present", string, values ["present","absent"]])
    ,("gid"                     , [integer])
    ,("ia_load_module"          , [string])
    ,("members"                 , [strings])
    ,("name"                    , [nameval])
    ,("provider"                , [string, values ["aix","directoryservice","groupadd","ldap","pw","window_adsi"]])
    ,("system"                  , [string, defaultvalue "false", values ["true","false"]])
    ]
