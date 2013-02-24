module Puppet.NativeTypes.Host (nativeHost) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isAlphaNum)
import qualified Data.Text as T

nativeHost :: (PuppetTypeName, PuppetTypeMethods)
nativeHost = ("host", PuppetTypeMethods validateHost parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions =
    [("comment"      , [string, values ["true","false"]])
    ,("ensure"       , [defaultvalue "present", string, values ["present","absent"]])
    ,("host_aliases" , [rarray, strings, checkhostname])
    ,("ip"           , [string, mandatory, ipaddr])
    ,("name"         , [nameval, checkhostname])
    ,("provider"     , [string, values ["parsed"]])
    ,("target"       , [string, fullyQualified])
    ]

validateHost :: PuppetTypeValidate
validateHost = defaultValidate parameterset >=> parameterFunctions parameterfunctions

checkhostname :: T.Text -> PuppetTypeValidate
checkhostname param res = case Map.lookup param (rrparams res) of
    Nothing                   -> Right res
    Just (ResolvedArray xs)   -> foldM (checkhostname' param) res xs
    Just x@(ResolvedString _) -> checkhostname' param res x
    Just x                    -> Left $ T.unpack param ++ " should be an array or a single string, not " ++ show x

checkhostname' :: T.Text -> RResource -> ResolvedValue -> Either String RResource
checkhostname' prm _   (ResolvedString "") = Left $ "Empty hostname for parameter " ++ T.unpack prm
checkhostname' prm res (ResolvedString x ) = checkhostname'' prm res x
checkhostname' prm _   x                   = Left $ "Parameter " ++ T.unpack prm ++ "should be an string or an array of strings, but this was found : " ++ show x

checkhostname'' :: T.Text -> RResource -> T.Text -> Either String RResource
checkhostname'' prm _   "" = Left $ "Empty hostname part in parameter " ++ T.unpack prm
checkhostname'' prm res prt =
    let (cur,nxt) = T.break (=='.') prt
        nextfunc = if T.null nxt
                        then Right res
                        else checkhostname'' prm res (T.tail nxt)
    in if T.null cur || (T.head cur == '-') || not (T.all (\x -> isAlphaNum x || (x=='-')) cur)
            then Left $ "Invalid hostname part for parameter " ++ T.unpack prm
            else nextfunc

