module Puppet.NativeTypes.Mount (nativeMount) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.HashSet as HS
import qualified Data.Text as T

nativeMount :: (PuppetTypeName, PuppetTypeMethods)
nativeMount = ("mount", PuppetTypeMethods validateMount parameterset)

parameterset :: HS.HashSet T.Text
parameterset = HS.fromList $ map fst parameterfunctions

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
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

validateMount :: PuppetTypeValidate
validateMount = defaultValidate parameterset >=> parameterFunctions parameterfunctions

