module Puppet.Language.NativeTypes.Concat (
    nativeConcat
  , nativeConcatFragment
) where

import Puppet.Language.NativeTypes.Helpers

nativeConcat :: (NativeTypeName, NativeTypeMethods)
nativeConcat = ("concat", nativetypemethods concatparamfunctions pure)

nativeConcatFragment :: (NativeTypeName, NativeTypeMethods)
nativeConcatFragment = ("concat::fragment", nativetypemethods fragmentparamfunctions validateSourceOrContent)

concatparamfunctions :: [(Text, [Text -> NativeTypeValidate])]
concatparamfunctions =
    [("name"                , [nameval])
    ,("ensure"              , [defaultvalue "present", string, values ["present","absent"]])
    ,("path"                , [string])
    ,("owner"               , [string])
    ,("group"               , [string])
    ,("validate_cmd"        , [string])
    ,("mode"                , [defaultvalue "0644", string])
    ,("warn"                , [defaultvalue "false", string, values ["false", "true"]])
    ,("force"               , [defaultvalue "false", string, values ["false", "true"]])
    ,("backup"              , [defaultvalue "puppet", string])
    ,("replace"             , [defaultvalue "true", string, values ["false", "true"]])
    ,("order"               , [defaultvalue "alpha", string, values ["alpha","numeric"]])
    ,("ensure_newline"      , [defaultvalue "false", string, values ["false", "true"]])
    -- deprecated
    -- ,("gnu"                 , [string])
    ]

fragmentparamfunctions :: [(Text, [Text -> NativeTypeValidate])]
fragmentparamfunctions =
    [("name"                , [nameval])
    ,("target"              , [string, mandatory])
    ,("content"             , [string])
    ,("source"              , [string])
     -- order should be an int or a string
    ,("order"               , [defaultvalue "10", string])
    ]
