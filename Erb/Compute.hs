module Erb.Compute(computeTemplate, getTemplateFile) where

import Data.List
import Puppet.Interpreter.Types
import System.Process
import System.IO
import Control.Monad.Error

computeTemplate :: String -> String -> [(String, GeneralValue)] -> IO (Either String String)
computeTemplate filename curcontext variables = do
    let rubyvars = "{\n" ++ intercalate ",\n" (concatMap toRuby variables ) ++ "\n}\n"
    (mstdin, mstdout, mstderr, phandle) <- runInteractiveCommand "ruby /home/smarechal/gits/puppet/language-puppet/Erb/test.rb"
    hPutStrLn mstdin (show $ length rubyvars)
    hPutStrLn mstdin curcontext
    hPutStr mstdin rubyvars
    hPutStrLn mstdin filename
    putStrLn curcontext
    putStrLn (show $ length rubyvars)
    putStr rubyvars
    putStrLn filename
    outsize <- hGetLine mstdout
    putStrLn outsize
    return $ Right outsize

getTemplateFile :: String -> CatalogMonad String
getTemplateFile rawpath = do
    throwError "gni"

toRuby (varname, Left _) = []
toRuby (varname, Right ResolvedUndefined) = []
toRuby (varname, Right varval) = ["\t" ++ show varname ++ " => " ++ toRuby' varval]
toRuby' (ResolvedString str) = show str
toRuby' (ResolvedInt i) = show i
toRuby' (ResolvedBool True) = "true"
toRuby' (ResolvedBool False) = "false"
toRuby' (ResolvedArray rr) = "[" ++ intercalate ", " (map toRuby' rr) ++ "]"
toRuby' (ResolvedHash hh) = "{ " ++ intercalate ", " (map (\(varname, varval) -> show varname ++ " => " ++ toRuby' varval) hh) ++ " }"
toRuby' x = show x
