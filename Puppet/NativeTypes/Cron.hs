module Puppet.NativeTypes.Cron (nativeCron) where

import Puppet.NativeTypes.Helpers
import Control.Monad.Error
import Puppet.Interpreter.Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char

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
    ,("name"                , [string])
    ,("provider"            , [defaultvalue "crontab", string, values ["crontab"]])
    ,("special"             , [string])
    ,("target"              , [string])
    ,("user"                , [defaultvalue "root", string])
    ,("weekday"             , [vrange 0 7 ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]])
    ]

validateCron :: PuppetTypeValidate
validateCron = defaultValidate parameterset >=> parameterFunctions parameterfunctions

vrange :: Integer -> Integer -> [String] -> String -> PuppetTypeValidate
vrange mi ma valuelist param res = case (Map.lookup param (rrparams res)) of
    Just (ResolvedArray xs) -> foldM (vrange' mi ma valuelist param) res xs
    Just x                  -> vrange' mi ma valuelist param res x
    Nothing                 -> defaultvalue "*" param res

vrange' :: Integer -> Integer -> [String] -> String -> RResource -> ResolvedValue -> Either String RResource
vrange' mi ma valuelist param res y = case y of
    ResolvedString "*"      -> Right res
    ResolvedString "absent" -> Right res
    ResolvedString x -> if elem x valuelist
        then Right res
        else parseval x mi ma param res
    ResolvedInt i -> checkint' i mi ma param res
    x  -> Left $ "Parameter " ++ param ++ " value should be a valid cron declaration and not " ++ show x

parseval :: String -> Integer -> Integer -> String -> PuppetTypeValidate
parseval ('*':'/':xs)    _ ma pname res = checkint xs      1 ma pname res
parseval resval         mi ma pname res = checkint resval mi ma pname res

checkint :: String -> Integer -> Integer -> String -> PuppetTypeValidate
checkint st mi ma pname res = if all isDigit st
    then
        let v = read st :: Integer
        in checkint' v mi ma pname res
    else Left $ "Invalid value type for parameter " ++ pname

checkint' :: Integer -> Integer -> Integer -> String -> PuppetTypeValidate
checkint' i mi ma param res = 
    if (i>=mi) && (i<=ma)
        then Right res
        else Left $ "Parameter " ++ param ++ " value is out of bound, should statisfy " ++ show mi ++ "<=" ++ show i ++ "<=" ++ show ma
