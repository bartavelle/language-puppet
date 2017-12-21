module Puppet.Language.NativeTypes.Host (nativeHost) where

import qualified Data.Char                           as Char
import qualified Data.Text                           as Text

import           Puppet.Language.NativeTypes.Helpers

nativeHost :: (NativeTypeName, NativeTypeMethods)
nativeHost = ("host", nativetypemethods parameterfunctions return)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
    [("comment"      , [string, values ["true","false"]])
    ,("ensure"       , [defaultvalue "present", string, values ["present","absent"]])
    ,("host_aliases" , [rarray, strings, checkhostname])
    ,("ip"           , [string, mandatory, ipaddr])
    ,("name"         , [nameval, checkhostname])
    ,("provider"     , [string, values ["parsed"]])
    ,("target"       , [string, fullyQualified])
    ]

checkhostname :: Text -> NativeTypeValidate
checkhostname param res = case res ^. rattributes . at param of
    Nothing            -> Right res
    Just (PArray xs)   -> foldM (checkhostname' param) res xs
    Just x@(PString _) -> checkhostname' param res x
    Just x             -> perror $ paramname param <+> "should be an array or a single string, not" <+> pretty x

checkhostname' :: Text -> Resource -> PValue -> Either PrettyError Resource
checkhostname' prm _   (PString "") = perror $ "Empty hostname for parameter" <+> paramname prm
checkhostname' prm res (PString x ) = checkhostname'' prm res x
checkhostname' prm _   x            = perror $ "Parameter " <+> paramname prm <+> "should be an string or an array of strings, but this was found :" <+> pretty x

checkhostname'' :: Text -> Resource -> Text -> Either PrettyError Resource
checkhostname'' prm _   "" = perror $ "Empty hostname part in parameter" <+> paramname prm
checkhostname'' prm res prt =
    let (cur,nxt) = Text.break (=='.') prt
        nextfunc = if Text.null nxt
                        then Right res
                        else checkhostname'' prm res (Text.tail nxt)
    in if Text.null cur || (Text.head cur == '-') || not (Text.all (\x -> Char.isAlphaNum x || (x=='-')) cur)
            then perror $ "Invalid hostname part for parameter" <+> paramname prm
            else nextfunc
