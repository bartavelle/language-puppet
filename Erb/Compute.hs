{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module Erb.Compute(computeTemplate, initTemplateDaemon) where

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Puppet.Interpreter.Types
import Puppet.Preferences
import Puppet.Stats
import Puppet.PP
import Puppet.Utils

import qualified Data.Either.Strict as S
import Control.Monad.Error
import Control.Concurrent
import System.Posix.Files
import Paths_language_puppet (getDataFileName)
import Erb.Parser
import Erb.Evaluate
import Erb.Ruby
import Debug.Trace
import qualified System.Log.Logger as LOG
import qualified Data.Text as T
import Text.Parsec hiding (string)
import Text.Parsec.Error
import Text.Parsec.Pos
import System.Environment
import Data.FileCache

import Control.Lens
import Data.Tuple.Strict
import qualified Foreign.Ruby as FR
import Foreign.Ruby.Safe


newtype TemplateParseError = TemplateParseError { tgetError :: ParseError }

instance Error TemplateParseError where
    noMsg = TemplateParseError $ newErrorUnknown (initialPos "dummy")
    strMsg s = TemplateParseError $ newErrorMessage (Message s) (initialPos "dummy")

type TemplateQuery = (Chan TemplateAnswer, Either T.Text T.Text, T.Text, Container ScopeInformation)
type TemplateAnswer = S.Either PrettyError T.Text

showRubyError :: RubyError -> PrettyError
showRubyError (Stack msg stk) = PrettyError $ dullred (string msg) </> dullyellow (string stk)
showRubyError (WithOutput str _) = PrettyError $ dullred (string str)

initTemplateDaemon :: RubyInterpreter -> (Preferences IO) -> MStats -> IO (Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either PrettyError T.Text))
initTemplateDaemon intr (Preferences _ modpath templatepath _ _ _ _ _) mvstats = do
    controlchan <- newChan
    templatecache <- newFileCache
    let returnError rs = return $ \_ _ _ -> return (S.Left (showRubyError rs))
    getRubyScriptPath "hrubyerb.rb" >>= loadFile intr >>= \case
        Left rs -> returnError rs
        Right () -> registerGlobalFunction4 intr "varlookup" hrresolveVariable >>= \case
            Right () -> do
                void $ forkIO $ templateDaemon intr (T.pack modpath) (T.pack templatepath) controlchan mvstats templatecache
                return (templateQuery controlchan)
            Left rs -> returnError rs

templateQuery :: Chan TemplateQuery -> Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either PrettyError T.Text)
templateQuery qchan filename scope variables = do
    rchan <- newChan
    writeChan qchan (rchan, filename, scope, variables)
    readChan rchan

templateDaemon :: RubyInterpreter -> T.Text -> T.Text -> Chan TemplateQuery -> MStats -> FileCacheR TemplateParseError [RubyStatement] -> IO ()
templateDaemon intr modpath templatepath qchan mvstats filecache = do
    nameThread "RubyTemplateDaemon"
    (respchan, fileinfo, scope, variables) <- readChan qchan
    case fileinfo of
        Right filename -> do
            let prts = T.splitOn "/" filename
                searchpathes | length prts > 1 = [modpath <> "/" <> head prts <> "/templates/" <> T.intercalate "/" (tail prts), templatepath <> "/" <> filename]
                             | otherwise        = [templatepath <> "/" <> filename]
            acceptablefiles <- filterM (fileExist . T.unpack) searchpathes
            if null acceptablefiles
                then writeChan respchan (S.Left $ PrettyError $ "Can't find template file for" <+> ttext filename <+> ", looked in" <+> list (map ttext searchpathes))
                else measure mvstats filename (computeTemplate intr (Right (head acceptablefiles)) scope variables mvstats filecache) >>= writeChan respchan
        Left _ -> measure mvstats "inline" (computeTemplate intr fileinfo scope variables mvstats filecache) >>= writeChan respchan
    templateDaemon intr modpath templatepath qchan mvstats filecache

computeTemplate :: RubyInterpreter -> Either T.Text T.Text -> T.Text -> Container ScopeInformation -> MStats -> FileCacheR TemplateParseError [RubyStatement] -> IO TemplateAnswer
computeTemplate intr fileinfo curcontext fvariables mstats filecache = do
    let (filename, ufilename) = case fileinfo of
                                    Left _ -> ("inline", "inline")
                                    Right x -> (x, T.unpack x)
        mkSafe a = makeSafe intr a >>= \case
            Left rr -> return (S.Left (showRubyError rr))
            Right x -> return x
        encapsulateError = _Left %~ TemplateParseError
        variables = fvariables & traverse . scopeVariables . traverse . _1 . _1 %~ toStr
        toStr (PNumber n) = PString (scientific2text n)
        toStr x = x
    traceEventIO ("START template " ++ T.unpack filename)
    parsed <- case fileinfo of
                  Right _      -> measure mstats ("parsing - " <> filename) $ lazyQuery filecache ufilename $ fmap encapsulateError (parseErbFile ufilename)
                  Left content -> measure mstats ("parsing - " <> filename) $ return $ encapsulateError (runParser erbparser () "inline" (T.unpack content))
    o <- case parsed of
        Left err -> do
            let !msg = "template " ++ ufilename ++ " could not be parsed " ++ show (tgetError err)
            traceEventIO msg
            LOG.debugM "Erb.Compute" msg
            measure mstats ("ruby - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (S.Right ev)
                Left err -> do
                    let !msg = "template " ++ ufilename ++ " evaluation failed " ++ show err
                    traceEventIO msg
                    LOG.debugM "Erb.Compute" msg
                    measure mstats ("ruby efail - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables
    traceEventIO ("STOP template " ++ T.unpack filename)
    return o

getRubyScriptPath :: String -> IO String
getRubyScriptPath rubybin = do
    let checkpath :: FilePath -> (IO FilePath) -> IO FilePath
        checkpath fp nxt = do
            e <- fileExist fp
            if e
                then return fp
                else nxt
        withExecutablePath = do
            path <- fmap (T.unpack . takeDirectory . T.pack) getExecutablePath
            let fullpath = path <> "/" <> rubybin
            checkpath fullpath $ checkpath ("/usr/local/bin/" <> rubybin) (return rubybin)
    cabalPath <- getDataFileName $ "ruby/" ++ rubybin :: IO FilePath
    checkpath cabalPath withExecutablePath

-- This must be called from the proper thread. As this is callback, this
-- should be ok.
hrresolveVariable :: RValue -> RValue -> RValue -> RValue -> IO RValue
-- T.Text -> Container PValue -> RValue -> RValue -> IO RValue
hrresolveVariable _ rscp rvariables rtoresolve = do
    scope <- FR.extractHaskellValue rscp
    variables <- FR.extractHaskellValue rvariables
    toresolve <- FR.fromRuby rtoresolve
    let answer = case toresolve of
                     Just "~g~e~t_h~a~s~h~" ->
                        let getvars ctx = (variables ^. ix ctx . scopeVariables) & traverse %~ view (_1 . _1)
                            vars = getvars "::" <> getvars scope
                        in  Right (PHash vars)
                     Just t -> getVariable variables scope t
                     _ -> Left "The variable name is not a string"
    case answer of
        Left _  -> FR.getSymbol "undef"
        Right r -> FR.toRuby r

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables = FR.freezeGC $ eitherDocIO $ do
    rscp <- FR.embedHaskellValue curcontext
    rvariables <- FR.embedHaskellValue variables
    let varlist = variables ^. ix curcontext . scopeVariables
    -- must be called from a "makeSafe" thingie
    let withBinding f = do
            erbBinding <- FR.safeMethodCall "ErbBinding" "new" [rscp,rvariables]
            case erbBinding of
                Left x -> return (Left x)
                Right v -> do
                     forM_ (itoList varlist) $ \(varname, (varval :!: _ :!: _)) -> FR.toRuby varval >>= FR.rb_iv_set v (T.unpack varname)
                     f v
    o <- case fileinfo of
             Right fname  -> do
                 rfname <- FR.toRuby fname
                 withBinding $ \v -> FR.safeMethodCall "Controller" "runFromFile" [rfname,v]
             Left content -> withBinding $ \v -> FR.toRuby content >>= FR.safeMethodCall "Controller" "runFromContent" . (:[v])
    FR.freeHaskellValue rvariables
    FR.freeHaskellValue rscp
    case o of
        Left (rr, _) ->
            let fname = case fileinfo of
                            Right f -> T.unpack f
                            Left _  -> "inline_template"
            in  return (S.Left $ PrettyError (dullred (text rr) <+> "in" <+> dullgreen (text fname)))
        Right r -> FR.fromRuby r >>= \case
            Just result -> return (S.Right result)
            Nothing -> return (S.Left "Could not deserialiaze ruby output")

