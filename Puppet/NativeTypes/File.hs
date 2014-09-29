module Puppet.NativeTypes.File (nativeFile) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import Data.Char (isDigit)
import qualified Data.Text as T
import Control.Lens

nativeFile :: (PuppetTypeName, PuppetTypeMethods)
nativeFile = ("file", ptypemethods parameterfunctions (validateSourceOrContent >=> validateMode))


-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
parameterfunctions =
    [("backup"      , [string])
    ,("checksum"    , [values ["md5", "md5lite", "mtime", "ctime", "none"]])
    ,("content"     , [string])
    --,("ensure"      , [defaultvalue "present", string, values ["directory","file","present","absent","link"]])
    ,("ensure"      , [defaultvalue "present", string])
    ,("force"       , [string, values ["true","false"]])
    ,("group"       , [defaultvalue "root", string])
    ,("ignore"      , [strings])
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
    ,("source"      , [rarray, strings, flip runarray checkSource])
    ]

validateMode :: PuppetTypeValidate
validateMode res = do
    modestr <- case res ^. rattributes . at "mode" of
                  Just (PString s) -> return s
                  Just x -> throwError $ PrettyError ("Invalide mode type, should be a string " <+> pretty x)
                  Nothing -> throwError "Could not find mode!"
    when ((T.length modestr /= 3) && (T.length modestr /= 4)) (throwError "Invalid mode size")
    unless (T.all isDigit modestr) (throwError "The mode should only be made of digits")
    if T.length modestr == 3
        then return $ res & rattributes . at "mode" ?~ PString (T.cons '0' modestr)
        else return res


checkSource :: T.Text -> PValue -> PuppetTypeValidate
checkSource _ (PString x) res | "puppet://" `T.isPrefixOf` x = Right res
                              | "file://" `T.isPrefixOf` x = Right res
                              | otherwise = throwError "A source should start with either puppet:// or file://"
checkSource _ x _ = throwError $ PrettyError ("Expected a string, not" <+> pretty x)
