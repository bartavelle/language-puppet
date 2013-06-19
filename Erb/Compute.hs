{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Erb.Compute(computeTemplate, getTemplateFile, initTemplateDaemon) where

import Puppet.Interpreter.Types
import Puppet.Init
import Puppet.Stats
import Puppet.Utils

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
import Text.Parsec

#ifdef HRUBY
import Foreign
import Foreign.Ruby

type RegisteredGetvariable = RValue -> RValue -> RValue -> RValue -> IO RValue
foreign import ccall "wrapper" mkRegisteredGetvariable :: RegisteredGetvariable -> IO (FunPtr RegisteredGetvariable)

#else
import System.IO
import SafeProcess
import Data.List
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T
import qualified Data.Foldable as F
#endif

type TemplateQuery = (Chan TemplateAnswer, Either T.Text T.Text, T.Text, Map.Map T.Text GeneralValue)
type TemplateAnswer = Either String T.Text



initTemplateDaemon :: Prefs -> MStats -> IO (Either T.Text T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text))
initTemplateDaemon (Prefs _ modpath templatepath _ _ ps _ _) mvstats = do
    controlchan <- newChan
#ifdef HRUBY
    initialize
    s <- (getRubyScriptPath "hrubyerb.rb" >>= \p -> rb_load_protect p 0)
    unless (s == 0) $ do
        msg <- showErrorStack
        error msg
    rubyResolveFunction <- mkRegisteredGetvariable hrresolveVariable
    rb_define_global_function "varlookup" rubyResolveFunction 3
    forkIO (templateDaemon modpath templatepath controlchan mvstats)
#else
    replicateM_ ps (forkIO (templateDaemon modpath templatepath controlchan mvstats))
#endif
    return (templateQuery controlchan)

templateQuery :: Chan TemplateQuery -> Either T.Text T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text)
templateQuery qchan filename scope variables = do
    rchan <- newChan
    writeChan qchan (rchan, filename, scope, variables)
    readChan rchan

templateDaemon :: T.Text -> T.Text -> Chan TemplateQuery -> MStats -> IO ()
templateDaemon modpath templatepath qchan mvstats = do
    (respchan, fileinfo, scope, variables) <- readChan qchan
    case fileinfo of
        Right filename -> do
            let parts = T.splitOn "/" filename
                searchpathes | length parts > 1 = [modpath <> "/" <> head parts <> "/templates/" <> T.intercalate "/" (tail parts), templatepath <> "/" <> filename]
                             | otherwise        = [templatepath <> "/" <> filename]
            acceptablefiles <- filterM (fileExist . T.unpack) searchpathes
            if null acceptablefiles
                then writeChan respchan (Left $ "Can't find template file for " ++ T.unpack filename ++ ", looked in " ++ show searchpathes)
                else measure mvstats ("total - " <> filename) (computeTemplate (Right (head acceptablefiles)) scope variables mvstats) >>= writeChan respchan
        Left _ -> measure mvstats "total - inline" (computeTemplate fileinfo scope variables mvstats) >>= writeChan respchan
    templateDaemon modpath templatepath qchan mvstats

computeTemplate :: Either T.Text T.Text -> T.Text -> Map.Map T.Text GeneralValue -> MStats -> IO TemplateAnswer
computeTemplate fileinfo curcontext variables mstats = do
    let (filename, ufilename) = case fileinfo of
                                    Left _ -> ("inline", "inline")
                                    Right x -> (x, T.unpack x)
    parsed <- case fileinfo of
                  Right _      -> measure mstats ("parsing - " <> filename) $ parseErbFile ufilename
                  Left content -> measure mstats ("parsing - " <> filename) (return (runParser erbparser () "inline" (T.unpack content)))
    case parsed of
        Left err -> do
            let !msg = "template " ++ ufilename ++ " could not be parsed " ++ show err
            traceEventIO msg
            LOG.debugM "Erb.Compute" msg
            measure mstats ("ruby - " <> filename) $ computeTemplateWRuby fileinfo curcontext variables
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (Right ev)
                Left err -> do
                    let !msg = "template " ++ ufilename ++ " evaluation failed " ++ show err
                    traceEventIO msg
                    LOG.debugM "Erb.Compute" msg
                    measure mstats ("ruby efail - " <> filename) $ computeTemplateWRuby fileinfo curcontext variables

getTemplateFile :: T.Text -> CatalogMonad T.Text
getTemplateFile = throwError

getRubyScriptPath :: String -> IO String
getRubyScriptPath rubybin = do
    cabalPath <- getDataFileName $ "ruby/" ++ rubybin :: IO FilePath
    exists    <- fileExist cabalPath
    if exists
        then return cabalPath
        else do
            path <- fmap (T.unpack . takeDirectory . T.pack) mGetExecutablePath
            let fullpath = path <> "/" <> rubybin
            lexists <- fileExist cabalPath
            return $ if lexists
                         then fullpath
                         else rubybin

#ifdef HRUBY
hrresolveVariable :: RValue -> RValue -> RValue -> RValue -> IO RValue
-- T.Text -> Map.Map T.Text GeneralValue -> RValue -> RValue -> IO RValue
hrresolveVariable _ rscope rvariables rtoresolve = do
    scope <- extractHaskellValue rscope
    variables <- extractHaskellValue rvariables
    toresolve <- fromRuby rtoresolve
    let answer = case toresolve of
                     Just t -> getVariable variables scope t
                     _ -> Left "The variable name is not a string"
    case answer of
        Left _ -> getSymbol "undef"
        Right r -> toRuby r

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables = freezeGC $ do
    rscope <- embedHaskellValue curcontext
    rvariables <- embedHaskellValue variables
    o <- case fileinfo of
             Right fname  -> do
                 rfname <- toRuby fname
                 safeMethodCall "Controller" "runFromFile" [rfname,rscope,rvariables]
             Left content -> toRuby content >>= safeMethodCall "Controller" "runFromContent" . (:[])
    freeHaskellValue rvariables
    freeHaskellValue rscope
    case o of
        Left (rr, _) ->
            let fname = case fileinfo of
                            Right f -> T.unpack f
                            Left _  -> "inline_template"
            in  return (Left ("Error in " <> fname <> ":\n" <> rr))
        Right r -> fromRuby r >>= \x -> case x of
                                            Just result -> return (Right result)
                                            Nothing -> return (Left "Could not deserialiaze ruby output")

#else
saveTmpContent :: T.Text -> IO FilePath
saveTmpContent cnt = do
    (name, h) <- openTempFile "/tmp" "inline_template.erb"
    hClose h
    return name

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Map.Map T.Text GeneralValue -> Maybe (FunPtr RegisteredGetvariable) -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables = do
    (temp, filename) <- case fileinfo of
                            Right x  -> return (Nothing, x)
                            Left cnt -> do
                                tmpfile <- saveTmpContent cnt
                                return (Just tmpfile, "inline")
    let rubyvars = "{\n" <> mconcat (intersperse ",\n" (concatMap toRuby (Map.toList variables))) <> "\n}\n" :: T.Builder
        input = T.fromText curcontext <> "\n" <> T.fromText filename <> "\n" <> rubyvars :: T.Builder
        ufilename = T.unpack filename
    rubyscriptpath <- getRubyScriptPath "calcerb.rb"
    traceEventIO ("start running ruby" ++ ufilename)
    !ret <- safeReadProcessTimeout "ruby" [rubyscriptpath] (T.toLazyText input) 1000
    traceEventIO ("finished running ruby" ++ ufilename)
    F.forM_ temp removeLink
    case ret of
        Just (Right x) -> return $! Right x
        Just (Left er) -> do
            (tmpfilename, tmphandle) <- openTempFile "/tmp" "templatefail"
            TL.hPutStr tmphandle (T.toLazyText input)
            hClose tmphandle
            return $ Left $ er ++ " - for template " ++ ufilename ++ " input in " ++ tmpfilename
        Nothing -> return $ Left "Process did not terminate"

minterc :: T.Builder -> [T.Builder] -> T.Builder
minterc _ [] = mempty
minterc _ [a] = a
minterc !sep !(x:xs) = x <> foldl' minterc' mempty xs
    where
        minterc' !curbuilder !b  = curbuilder <> sep <> b

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
#endif
