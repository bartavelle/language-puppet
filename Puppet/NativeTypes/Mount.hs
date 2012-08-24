module Puppet.NativeTypes.Mount (nativeMount) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import qualified Data.Set as Set

nativeMount = ("mount", PuppetTypeMethods validateMount parameterset)

parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions =
    [("atboot"      , [string, values ["true","false"]])
    ,("blockdevice" , [string])
    ,("device"      , [string, mandatory])
    ,("dump"        , [integer, inrange 0 2])
    ,("ensure"      , [defaultvalue "present", string, values ["present","absent","mounted"]])
    ,("fstype"      , [string, mandatory])
    ,("name"        , [string])
    ,("options"     , [string])
    ,("pass"        , [defaultvalue "0", integer])
    ,("provider"    , [defaultvalue "parsed", string, values ["parsed"]])
    ,("remounts"    , [string, values ["true","false"]])
    ,("target"      , [string, fullyQualified])
    ]

validateMount :: PuppetTypeValidate
validateMount = defaultValidate parameterset >=> parameterFunctions parameterfunctions

