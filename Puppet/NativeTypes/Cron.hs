module Puppet.NativeTypes.Cron (nativeCron) where

import qualified Text.PrettyPrint.ANSI.Leijen as P
import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import Puppet.Utils
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Lens
import qualified Data.Vector as V

nativeCron :: (PuppetTypeName, PuppetTypeMethods)
nativeCron = ("cron", PuppetTypeMethods validateCron parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset :: HS.HashSet T.Text
parameterset = HS.fromList $ map fst parameterfunctions

parameterfunctions :: [(T.Text, [T.Text -> PuppetTypeValidate])]
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

validateCron :: PuppetTypeValidate
validateCron = defaultValidate parameterset >=> parameterFunctions parameterfunctions

vrange :: Integer -> Integer -> [T.Text] -> T.Text -> PuppetTypeValidate
vrange mi ma valuelist param res = case res ^. rattributes . at param of
    Just (PArray xs) -> V.foldM (vrange' mi ma valuelist param) res xs
    Just x                    -> vrange' mi ma valuelist param res x
    Nothing                   -> defaultvalue "*" param res

vrange' :: Integer -> Integer -> [T.Text] -> T.Text -> Resource -> PValue -> Either Doc Resource
vrange' mi ma valuelist param res y = case y of
    PString "*"      -> Right res
    PString "absent" -> Right res
    PString x -> if x `elem` valuelist
        then Right res
        else parseval x mi ma param res
    x  -> Left $ "Parameter" <+> paramname param <+> "value should be a valid cron declaration and not" <+> pretty x

parseval :: T.Text -> Integer -> Integer -> T.Text -> PuppetTypeValidate
parseval resval mi ma pname res | "*/" `T.isPrefixOf` resval = checkint (T.drop 2 resval)  1 ma pname res
                                | otherwise                  = checkint resval            mi ma pname res

checkint :: T.Text -> Integer -> Integer -> T.Text -> PuppetTypeValidate
checkint st mi ma pname res =
    case readDecimal st of
        Right v -> checkint' v mi ma pname res
        Left rr -> Left $ "Invalid value type for parameter" <+> paramname pname <+> ": " <+> red (text rr)

checkint' :: Integer -> Integer -> Integer -> T.Text -> PuppetTypeValidate
checkint' i mi ma param res =
    if (i>=mi) && (i<=ma)
        then Right res
        else Left $ "Parameter" <+> paramname param <+> "value is out of bound, should statisfy" <+> P.integer mi <+> "<=" <+> P.integer i <+> "<=" <+> P.integer ma
