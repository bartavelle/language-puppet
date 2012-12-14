module Puppet.NativeTypes.ZoneRecord (nativeZoneRecord) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set

nativeZoneRecord = ("zone_record", PuppetTypeMethods validateZoneRecord parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions = 
    [("name"                , [nameval])
    ,("owner"               , [string])
    ,("dest"                , [string])
    ,("ensure"              , [defaultvalue "present", string, values ["present","absent"]])
    ,("rtype"               , [string, defaultvalue "A", values ["SOA", "A", "AAAA", "MX", "NS", "CNAME", "PTR", "SRV"]])
    ,("rclass"              , [defaultvalue "IN", string])
    ,("ttl"                 , [defaultvalue "2d", string])
    ,("target"              , [string, mandatory])
    ,("nsname"              , [string])
    ,("serial"              , [string])
    ,("slave_refresh"       , [string])
    ,("slave_retry"         , [string])
    ,("slave_expiration"    , [string])
    ,("min_ttl"             , [string])
    ,("email"               , [string])
    ]

validateZoneRecord :: PuppetTypeValidate
validateZoneRecord = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> validateMandatories

validateMandatories :: PuppetTypeValidate
validateMandatories res = case (Map.lookup "rtype" (rrparams res)) of
    Nothing                     -> Left "The rtype parameter is mandatory."
    Just (ResolvedString "SOA") -> foldM (\r n -> mandatory n r) res ["nsname", "email", "serial", "slave_refresh", "slave_retry", "slave_expiration", "min_ttl"]
    Just (ResolvedString "NS")  -> foldM (\r n -> mandatory n r) res ["owner", "rclass", "rtype", "dest"]
    Just (ResolvedString _)     -> foldM (\r n -> mandatory n r) res ["owner", "rclass", "rtype", "dest", "ttl"]
    Just x                      -> Left $ "Can't use this for the rtype parameter " ++ show x
