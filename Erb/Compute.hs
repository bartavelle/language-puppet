module Erb.Compute(computeTemplate) where

import Data.List
import Puppet.Interpreter.Types

computeTemplate :: String -> [(String, GeneralValue)] -> IO String
computeTemplate filename variables = do
    let rubyvars = "vars = {\n" ++ intercalate ",\n" (map toRuby variables ) ++ "\n}"
    putStrLn rubyvars
    return rubyvars

toRuby (varname, Left _) = ""
toRuby (varname, Right varval) = "\t" ++ show varname ++ " => " ++ toRuby' varval
toRuby' (ResolvedString str) = show str
toRuby' (ResolvedInt i) = show i
toRuby' (ResolvedBool True) = "true"
toRuby' (ResolvedBool False) = "false"
toRuby' (ResolvedArray rr) = show $ map toRuby' rr
toRuby' (ResolvedHash hh) = "{ " ++ intercalate ", " (map (\(varname, varval) -> show varname ++ " => " ++ toRuby' varval) hh) ++ " }"
toRuby' x = show x
