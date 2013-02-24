module Puppet.NativeTypes.Cron (nativeCron) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char
import qualified Data.Text as T

nativeCron :: (PuppetTypeName, PuppetTypeMethods)
nativeCron = ("cron", PuppetTypeMethods validateCron parameterset)

-- Autorequires: If Puppet is managing the user or group that owns a file, the file resource will autorequire them. If Puppet is managing any parent directories of a file, the file resource will autorequire them.
parameterset = Set.fromList $ map fst parameterfunctions
parameterfunctions =
    [("command"             , [string, mandatory])
    ,("ensure"              , [defaultvalue "present", string, values ["present","absent"]])
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
    ,("weekday"             , [vrange 0 7 ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]])
    ]

validateCron :: PuppetTypeValidate
validateCron = defaultValidate parameterset >=> parameterFunctions parameterfunctions

vrange :: Integer -> Integer -> [T.Text] -> T.Text -> PuppetTypeValidate
vrange mi ma valuelist param res = case (Map.lookup param (rrparams res)) of
    Just (ResolvedArray xs) -> foldM (vrange' mi ma valuelist param) res xs
    Just x                  -> vrange' mi ma valuelist param res x
    Nothing                 -> defaultvalue "*" param res

vrange' :: Integer -> Integer -> [T.Text] -> T.Text -> RResource -> ResolvedValue -> Either String RResource
vrange' mi ma valuelist param res y = case y of
    ResolvedString "*"      -> Right res
    ResolvedString "absent" -> Right res
    ResolvedString x -> if elem x valuelist
        then Right res
        else parseval x mi ma param res
    ResolvedInt i -> checkint' i mi ma param res
    x  -> Left $ "Parameter " ++ T.unpack param ++ " value should be a valid cron declaration and not " ++ show x

parseval :: T.Text -> Integer -> Integer -> T.Text -> PuppetTypeValidate
parseval resval mi ma pname res | T.isPrefixOf "*/" resval = checkint (T.drop 2 resval)  1 ma pname res
                                | otherwise                = checkint resval            mi ma pname res

checkint :: T.Text -> Integer -> Integer -> T.Text -> PuppetTypeValidate
checkint st mi ma pname res = if T.all isDigit st
    then
        let v = read (T.unpack st) :: Integer
        in checkint' v mi ma pname res
    else Left $ "Invalid value type for parameter " ++ T.unpack pname

checkint' :: Integer -> Integer -> Integer -> T.Text -> PuppetTypeValidate
checkint' i mi ma param res =
    if (i>=mi) && (i<=ma)
        then Right res
        else Left $ "Parameter " ++ T.unpack param ++ " value is out of bound, should statisfy " ++ show mi ++ "<=" ++ show i ++ "<=" ++ show ma
