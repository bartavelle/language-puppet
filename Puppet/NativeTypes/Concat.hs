module Puppet.NativeTypes.Concat (
    nativeConcat
  , nativeConcatFragment
) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.HashSet as HS
import qualified Data.Text as T

nativeConcat :: (PuppetTypeName, PuppetTypeMethods)
nativeConcat = ("concat", PuppetTypeMethods validateConcat concatparams)

nativeConcatFragment :: (PuppetTypeName, PuppetTypeMethods)
nativeConcatFragment = ("concat::fragment", PuppetTypeMethods validateFragment fragmentparams)

concatparams :: HS.HashSet T.Text
concatparams = HS.fromList $ map fst concatparamdef

fragmentparams :: HS.HashSet T.Text
fragmentparams = HS.fromList $ map fst fragmentparamdef

concatparamdef :: [(T.Text, [T.Text -> PuppetTypeValidate])]
concatparamdef =
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

fragmentparamdef :: [(T.Text, [T.Text -> PuppetTypeValidate])]
fragmentparamdef =
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

validateConcat :: PuppetTypeValidate
validateConcat = defaultValidate concatparams >=> parameterFunctions concatparamdef

validateFragment :: PuppetTypeValidate
validateFragment = defaultValidate fragmentparams >=> parameterFunctions fragmentparamdef >=> validateSourceOrContent
