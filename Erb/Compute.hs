module Erb.Compute(computeTemplate, getTemplateFile, initTemplateDaemon) where

import Data.List
import Puppet.Interpreter.Types
import Puppet.Init
import SafeProcess
import System.IO
import qualified Data.List.Utils as DLU
import Control.Monad.Error
import Control.Concurrent
import System.Posix.Files
import Paths_language_puppet (getDataFileName)

type TemplateQuery = (Chan TemplateAnswer, String, String, [(String, GeneralValue)])
type TemplateAnswer = Either String String

initTemplateDaemon :: Prefs -> IO (String -> String -> [(String, GeneralValue)] -> IO (Either String String))
initTemplateDaemon (Prefs _ modpath templatepath _ _) = do
    controlchan <- newChan
    forkIO (templateDaemon modpath templatepath controlchan)
    return (templateQuery controlchan)

templateQuery :: Chan TemplateQuery -> String -> String -> [(String, GeneralValue)] -> IO (Either String String)
templateQuery qchan filename scope variables = do
    rchan <- newChan
    writeChan qchan (rchan, filename, scope, variables)
    readChan rchan

templateDaemon :: String -> String -> Chan TemplateQuery -> IO ()
templateDaemon modpath templatepath qchan = do
    (respchan, filename, scope, variables) <- readChan qchan
    let parts = DLU.split "/" filename
        searchpathes | length parts > 1 = [modpath ++ "/" ++ head parts ++ "/templates/" ++ (DLU.join "/" (tail parts)), templatepath ++ "/" ++ filename]
                     | otherwise        = [templatepath ++ "/" ++ filename]
    acceptablefiles <- filterM fileExist searchpathes
    if(null acceptablefiles)
        then writeChan respchan (Left $ "Can't find template file for " ++ filename ++ ", looked in " ++ show searchpathes)
        else computeTemplate (head acceptablefiles) scope variables >>= writeChan respchan
    templateDaemon modpath templatepath qchan

computeTemplate :: String -> String -> [(String, GeneralValue)] -> IO TemplateAnswer
computeTemplate filename curcontext variables = do
    let rubyvars = "{\n" ++ intercalate ",\n" (concatMap toRuby variables ) ++ "\n}\n"
        input = curcontext ++ "\n" ++ filename ++ "\n" ++ rubyvars
    rubyscriptpath <- getDataFileName "ruby/calcerb.rb"
    ret <- safeReadProcessTimeout "ruby" [rubyscriptpath] input 1000
    case ret of
        Just (Right x) -> return $ Right x
        Just (Left er) -> do
            (tmpfilename, tmphandle) <- openTempFile "/tmp" "templatefail"
            hPutStr tmphandle input
            hClose tmphandle
            return $ Left $ er ++ " - for template " ++ filename ++ " input in " ++ tmpfilename
        Nothing -> do
            return $ Left "Process did not terminate"

getTemplateFile :: String -> CatalogMonad String
getTemplateFile rawpath = do
    throwError rawpath

toRuby (_, Left _) = []
toRuby (_, Right ResolvedUndefined) = []
toRuby (varname, Right varval) = ["\t" ++ show varname ++ " => " ++ toRuby' varval]
toRuby' (ResolvedString str) = show str
toRuby' (ResolvedInt i) = "'" ++ show i ++ "'"
toRuby' (ResolvedBool True) = "true"
toRuby' (ResolvedBool False) = "false"
toRuby' (ResolvedArray rr) = "[" ++ intercalate ", " (map toRuby' rr) ++ "]"
toRuby' (ResolvedHash hh) = "{ " ++ intercalate ", " (map (\(varname, varval) -> show varname ++ " => " ++ toRuby' varval) hh) ++ " }"
toRuby' x = show x
