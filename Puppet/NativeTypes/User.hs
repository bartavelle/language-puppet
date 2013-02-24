module Puppet.NativeTypes.User (nativeUser) where

import Puppet.NativeTypes.Helpers
import Puppet.Interpreter.Types
import Control.Monad.Error
import qualified Data.Set as Set

nativeUser :: (PuppetTypeName, PuppetTypeMethods)
nativeUser = ("user", PuppetTypeMethods validateUser parameterset)

-- Autorequires: If Puppet is managing the user or user that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions =
    [("allowdupe"               , [string, defaultvalue "false", values ["true","false"]])
    ,("attribute_membership"    , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("attributes"              , [rarray,strings])
    ,("auth_membership"         , [defaultvalue "true"])
    ,("auths"                   , [rarray,strings])
    ,("comment"                 , [string])
    ,("ensure"                  , [defaultvalue "present", string, values ["present","absent","role"]])
    ,("expiry"                  , [string])
    ,("gid"                     , [string])
    ,("groups"                  , [rarray,strings])
    ,("home"                    , [string, fullyQualified, noTrailingSlash])
    ,("ia_load_module"          , [string])
    ,("iterations"              , [integer])
    ,("key_membership"          , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("keys"                    , [])
    ,("managehome"              , [string, defaultvalue "false", values ["true","false"]])
    ,("membership"              , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("name"                    , [nameval])
    ,("password"                , [string])
    ,("password_max_age"        , [integer])
    ,("password_min_age"        , [integer])
    ,("profile_membership"      , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("profiles"                , [rarray,strings])
    ,("project"                 , [string])
    ,("provider"                , [string, values ["aix","directoryservice","hpuxuseradd","useradd","ldap","pw","user_role_add","window_adsi"]])
    ,("role_membership"         , [string, defaultvalue "minimum", values ["inclusive","minimum"]])
    ,("roles"                   , [rarray,strings])
    ,("salt"                    , [string])
    ,("shell"                   , [string, fullyQualified, noTrailingSlash])
    ,("system"                  , [string, defaultvalue "false", values ["true","false"]])
    ,("uid"                     , [integer])
    ]

validateUser :: PuppetTypeValidate
validateUser = defaultValidate parameterset >=> parameterFunctions parameterfunctions

