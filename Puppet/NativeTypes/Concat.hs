module Puppet.NativeTypes.Concat (
    nativeConcat
  , nativeConcatFragment
) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import qualified Data.Text as T

nativeConcat :: (NativeTypeName, NativeTypeMethods)
nativeConcat = ("concat", ptypemethods concatparamfunctions return)

nativeConcatFragment :: (NativeTypeName, NativeTypeMethods)
nativeConcatFragment = ("concat::fragment", ptypemethods fragmentparamfunctions validateSourceOrContent)

concatparamfunctions :: [(T.Text, [T.Text -> NativeTypeValidate])]
concatparamfunctions =
    [("name"                , [nameval])
    ,("ensure"              , [defaultvalue "present", string, values ["present","absent"]])
    ,("path"                , [string])
    ,("owner"               , [string])
    ,("group"               , [string])
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

fragmentparamfunctions :: [(T.Text, [T.Text -> NativeTypeValidate])]
fragmentparamfunctions =
    [("name"                , [nameval])
    ,("target"              , [string, mandatory])
    ,("content"             , [string])
    ,("source"              , [string])
     -- order should be an int or a string
    ,("order"               , [defaultvalue "10", string])
    ,("ensure"              , [string, values ["present","absent"]])
   -- deprecated field
   -- ,("mode"                , [string])
   -- ,("owner"               , [string])
   -- ,("group"               , [string])
   -- ,("backup"              , [string])
    ]
