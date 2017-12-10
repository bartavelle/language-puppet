module Puppet.NativeTypes.Cron
       (nativeCron)
where

import           Puppet.Prelude

import qualified Data.Text                    as Text
import qualified Data.Vector                  as V
import qualified Text.PrettyPrint.ANSI.Leijen as P

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes.Helpers

nativeCron :: (NativeTypeName, NativeTypeMethods)
nativeCron = ("cron", nativetypemethods parameterfunctions return )

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterfunctions :: [(Text, [Text -> NativeTypeValidate])]
parameterfunctions =
    [("ensure"              , [defaultvalue "present", string, values ["present","absent"]])
    ,("command"             , [string, mandatoryIfNotAbsent])
    ,("environment"         , [])
    ,("hour"                , [vrange 0 23 [] ])
    ,("minute"              , [vrange 0 59 [] ])
    ,("month"               , [vrange 1 12 ["January","February","March","April","May","June","July","August","September","October","November","December"] ])
    ,("monthday"            , [vrange 1 31 [] ])
    ,("name"                , [nameval])
    ,("provider"            , [defaultvalue "crontab", string, values ["crontab"]])
    ,("special"             , [string])
    ,("target"              , [string])
    ,("user"                , [defaultvalue "root", string])
    ,("weekday"             , [vrange 0 7 ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]])
    ]


vrange :: Integer -> Integer -> [Text] -> Text -> NativeTypeValidate
vrange mi ma valuelist param res = case res ^. rattributes . at param of
    Just (PArray xs) -> V.foldM (vrange' mi ma valuelist param) res xs
    Just x           -> vrange' mi ma valuelist param res x
    Nothing          -> defaultvalue "*" param res

vrange' :: Integer -> Integer -> [Text] -> Text -> Resource -> PValue -> Either PrettyError Resource
vrange' mi ma valuelist param res y = case y of
    PString "*"      -> Right res
    PString "absent" -> Right res
    PNumber n        -> checkint' n mi ma param res
    PString x -> if x `elem` valuelist
        then Right res
        else parseval x mi ma param res
    x  -> perror $ "Parameter" <+> paramname param <+> "value should be a valid cron declaration and not" <+> pretty x

parseval :: Text -> Integer -> Integer -> Text -> NativeTypeValidate
parseval resval mi ma pname res | "*/" `Text.isPrefixOf` resval = checkint (Text.drop 2 resval)  1 ma pname res
                                | otherwise                  = checkint resval            mi ma pname res

checkint :: Text -> Integer -> Integer -> Text -> NativeTypeValidate
checkint st mi ma pname res =
    case text2Scientific st of
        Just n  -> checkint' n mi ma pname res
        Nothing -> perror $ "Invalid value type for parameter" <+> paramname pname <+> ": " <+> red (ttext st)

checkint' :: Scientific -> Integer -> Integer -> Text -> NativeTypeValidate
checkint' i mi ma param res =
    if (i >= fromIntegral mi) && (i <= fromIntegral ma)
        then Right res
        else perror $ "Parameter" <+> paramname param <+> "value is out of bound, should satisfy" <+> P.integer mi <+> "<=" <+> P.string (show i) <+> "<=" <+> P.integer ma
