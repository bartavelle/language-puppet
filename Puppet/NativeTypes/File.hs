module Puppet.NativeTypes.File (nativeFile) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

nativeFile = [("file", PuppetTypeMethods validateFile parameterset)]

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions = 
    [("backup"      , [string])
    ,("checksum"    , [values ["md5", "md5lite", "mtime", "ctime", "none"]])
    ,("content"     , [string])
    ,("ensure"      , [defaultvalue "present", string])
    ,("force"       , [string, values ["true","false"]])
    ,("group"       , [defaultvalue "root", string])
    ,("ignore"      , [string])
    ,("links"       , [string])
    ,("mode"        , [integer])
    ,("owner"       , [string])
    ,("path"        , [string])
    ,("provider"    , [values ["posix","windows"]])
    ,("purge"       , [string, values ["true","false"]])
    ,("recurse"     , [string, values ["inf","true","false","remote"]])
    ,("recurselimit", [integer])
    ,("replace"     , [string, values ["true","false"]])
    ,("sourceselect", [values ["first","all"]])
    ,("target"      , [string])
    ,("source"      , [])
    ]

validateFile :: PuppetTypeValidate
validateFile = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> validateSourceOrContent


validateSourceOrContent :: PuppetTypeValidate
validateSourceOrContent res = let
    parammap = rrparams res
    source    = Map.member "source"  parammap
    content   = Map.member "content" parammap
    in if (source && content)
        then Left "Source and content can't be specified at the same time"
        else Right res
