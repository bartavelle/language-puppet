module Puppet.NativeTypes.File (nativeFile) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Char (isDigit)
import qualified Data.Text as T
import Control.Lens

nativeFile :: (PuppetTypeName, PuppetTypeMethods)
nativeFile = ("file", PuppetTypeMethods validateFile parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset :: HS.HashSet T.Text
parameterset = HS.fromList $ map fst parameterfunctions

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
parameterfunctions =
    [("backup"      , [string])
    ,("checksum"    , [values ["md5", "md5lite", "mtime", "ctime", "none"]])
    ,("content"     , [string])
    --,("ensure"      , [defaultvalue "present", string, values ["directory","file","present","absent","link"]])
    ,("ensure"      , [defaultvalue "present", string])
    ,("force"       , [string, values ["true","false"]])
    ,("group"       , [defaultvalue "root", string])
    ,("ignore"      , [string])
    ,("links"       , [string])
    ,("mode"        , [defaultvalue "0644", string])
    ,("owner"       , [string])
    ,("path"        , [nameval, fullyQualified, noTrailingSlash])
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
validateFile = defaultValidate parameterset >=> parameterFunctions parameterfunctions >=> validateSourceOrContent >=> validateMode

validateMode :: PuppetTypeValidate
validateMode res = let
    modestr = case res ^. rattributes . at "mode" of
                  Just (PString s) -> s
                  _ -> "0644"
    in do
        when ((T.length modestr /= 3) && (T.length modestr /= 4)) (throwError "Invalid mode size")
        unless (T.all isDigit modestr) (throwError "The mode should only be made of digits")
        if T.length modestr == 3
            then return $ res & rattributes . at "mode" ?~ PString (T.cons '0' modestr)
            else return res

validateSourceOrContent :: PuppetTypeValidate
validateSourceOrContent res = let
    parammap =  res ^. rattributes
    source    = HM.member "source"  parammap
    content   = HM.member "content" parammap
    in if source && content
        then Left "Source and content can't be specified at the same time"
        else Right res
