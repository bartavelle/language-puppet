module Puppet.NativeTypes.Host (nativeHost) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.HashSet as HS
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Control.Lens
import qualified Data.Vector as V

nativeHost :: (PuppetTypeName, PuppetTypeMethods)
nativeHost = ("host", PuppetTypeMethods validateHost parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset :: HS.HashSet T.Text
parameterset = HS.fromList $ map fst parameterfunctions

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
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
checkhostname param res = case res ^. rattributes . at param of
    Nothing                   -> Right res
    Just (PArray xs)   -> V.foldM (checkhostname' param) res xs
    Just x@(PString _) -> checkhostname' param res x
    Just x                    -> Left $ paramname param <+> "should be an array or a single string, not" <+> pretty x

checkhostname' :: T.Text -> Resource -> PValue -> Either Doc Resource
checkhostname' prm _   (PString "") = Left $ "Empty hostname for parameter" <+> paramname prm
checkhostname' prm res (PString x ) = checkhostname'' prm res x
checkhostname' prm _   x            = Left $ "Parameter " <+> paramname prm <+> "should be an string or an array of strings, but this was found :" <+> pretty x

checkhostname'' :: T.Text -> Resource -> T.Text -> Either Doc Resource
checkhostname'' prm _   "" = Left $ "Empty hostname part in parameter" <+> paramname prm
checkhostname'' prm res prt =
    let (cur,nxt) = T.break (=='.') prt
        nextfunc = if T.null nxt
                        then Right res
                        else checkhostname'' prm res (T.tail nxt)
    in if T.null cur || (T.head cur == '-') || not (T.all (\x -> isAlphaNum x || (x=='-')) cur)
            then Left $ "Invalid hostname part for parameter" <+> paramname prm
            else nextfunc

