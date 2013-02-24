module Erb.Compute(computeTemplate, getTemplateFile, initTemplateDaemon) where

import Puppet.Interpreter.Types
import Puppet.Init
import Puppet.Stats
import Puppet.Utils
import SafeProcess

import Data.List
import System.IO
import Control.Monad.Error
import Control.Concurrent
import System.Posix.Files
import Paths_language_puppet (getDataFileName)
import Erb.Parser
import Erb.Evaluate
import qualified Data.Map as Map
import Debug.Trace
import qualified System.Log.Logger as LOG
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T

type TemplateQuery = (Chan TemplateAnswer, T.Text, T.Text, Map.Map T.Text GeneralValue)
type TemplateAnswer = Either String T.Text

initTemplateDaemon :: Prefs -> MStats -> IO (T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text))
initTemplateDaemon (Prefs _ modpath templatepath _ _ ps _ _) mvstats = do
    controlchan <- newChan
    replicateM_ ps (forkIO (templateDaemon modpath templatepath controlchan mvstats))
    return (templateQuery controlchan)

templateQuery :: Chan TemplateQuery -> T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text)
templateQuery qchan filename scope variables = do
    rchan <- newChan
    writeChan qchan (rchan, filename, scope, variables)
    readChan rchan

templateDaemon :: T.Text -> T.Text -> Chan TemplateQuery -> MStats -> IO ()
templateDaemon modpath templatepath qchan mvstats = do
    (respchan, filename, scope, variables) <- readChan qchan
    let parts = T.splitOn "/" filename
        searchpathes | length parts > 1 = [modpath <> "/" <> head parts <> "/templates/" <> (T.intercalate "/" (tail parts)), templatepath <> "/" <> filename]
                     | otherwise        = [templatepath <> "/" <> filename]
    acceptablefiles <- filterM (fileExist . T.unpack) searchpathes
    if(null acceptablefiles)
        then writeChan respchan (Left $ "Can't find template file for " ++ T.unpack filename ++ ", looked in " ++ show searchpathes)
        else measure mvstats ("total - " <> filename) (computeTemplate (head acceptablefiles) scope variables mvstats) >>= writeChan respchan
    templateDaemon modpath templatepath qchan mvstats

computeTemplate :: T.Text -> T.Text -> Map.Map T.Text GeneralValue -> MStats -> IO TemplateAnswer
computeTemplate filename curcontext variables mstats = do
    let ufilename = T.unpack filename
    parsed <- measure mstats ("parsing - " <> filename) $ parseErbFile ufilename
    case parsed of
        Left err -> do
            let !msg = "template " ++ ufilename ++ " could not be parsed " ++ show err
            traceEventIO msg
            LOG.debugM "Erb.Compute" msg
            measure mstats ("ruby - " <> filename) $ computeTemplateWRuby filename curcontext variables
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (Right ev)
                Left err -> do
                    let !msg = "template " ++ ufilename ++ " evaluation failed " ++ show err
                    traceEventIO msg
                    LOG.debugM "Erb.Compute" msg
                    measure mstats ("ruby efail - " <> filename) $ computeTemplateWRuby filename curcontext variables

computeTemplateWRuby :: T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO TemplateAnswer
computeTemplateWRuby filename curcontext variables = do
    let rubyvars = "{\n" <> mconcat (intersperse ",\n" (concatMap toRuby (Map.toList variables))) <> "\n}\n" :: T.Builder
        input = T.fromText curcontext <> "\n" <> T.fromText filename <> "\n" <> rubyvars :: T.Builder
        ufilename = T.unpack filename
    rubyscriptpath <- do
        let rubybin = "calcerb.rb"
        cabalPath <- getDataFileName $ "ruby/" ++ T.unpack rubybin
        exists    <- fileExist cabalPath
        case exists of
            True -> return cabalPath
            False -> do
                path <- fmap (takeDirectory . T.pack) mGetExecutablePath
                let fullpath = path <> "/" <> rubybin
                lexists <- fileExist cabalPath
                case lexists of
                    True  -> return $ T.unpack fullpath
                    False -> return $ T.unpack rubybin
    traceEventIO ("start running ruby" ++ ufilename)
    !ret <- safeReadProcessTimeout "ruby" [rubyscriptpath] (T.toLazyText input) 1000
    traceEventIO ("finished running ruby" ++ ufilename)
    case ret of
        Just (Right x) -> return $! Right x
        Just (Left er) -> do
            (tmpfilename, tmphandle) <- openTempFile "/tmp" "templatefail"
            TL.hPutStr tmphandle (T.toLazyText input)
            hClose tmphandle
            return $ Left $ er ++ " - for template " ++ ufilename ++ " input in " ++ tmpfilename
        Nothing -> do
            return $ Left "Process did not terminate"

minterc :: T.Builder -> [T.Builder] -> T.Builder
minterc _ [] = mempty
minterc _ [a] = a
minterc !sep !(x:xs) = x <> foldl' minterc' mempty xs
    where
        minterc' !curbuilder !b  = curbuilder <> sep <> b

getTemplateFile :: T.Text -> CatalogMonad T.Text
getTemplateFile rawpath = do
    throwError rawpath
renderString :: T.Text -> T.Builder
renderString x = let !y = T.fromString (show x) in y

toRuby :: (T.Text, GeneralValue) -> [T.Builder]
toRuby (_, Left _) = []
toRuby (_, Right ResolvedUndefined) = []
toRuby (varname, Right varval) = ["\t" <> renderString varname <> " => " <> toRuby' varval]
toRuby' (ResolvedString str) = renderString str
toRuby' (ResolvedInt i) = "\'" <> T.decimal i <> "\'"
toRuby' (ResolvedBool True) = "true"
toRuby' (ResolvedBool False) = "false"
toRuby' (ResolvedArray rr) = "[" <> minterc ", " (map toRuby' rr) <> "]"
toRuby' (ResolvedHash hh) = "{ " <> minterc ", " (map (\(varname, varval) -> renderString varname <> " => " <> toRuby' varval) hh) <>  " }"
toRuby' ResolvedUndefined = ":undef"
toRuby' (ResolvedRReference rtype (ResolvedString rname)) = renderString ( rtype <> "[" <> rname <> "]" )
toRuby' x = T.fromString $ show x
