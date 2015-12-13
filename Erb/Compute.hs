{-# LANGUAGE CPP                      #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Erb.Compute (
    computeTemplate
  , initTemplateDaemon
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson.Lens
import qualified Data.Either.Strict           as S
import           Data.FileCache
import qualified Data.Text                    as T
import           Data.String
import qualified Data.Vector                  as V
import           Debug.Trace
import           Erb.Evaluate
import           Erb.Parser
import           Erb.Ruby
import           Paths_language_puppet        (getDataFileName)
import           System.Environment
import qualified System.Log.Logger            as LOG
import           System.Posix.Files
import           Text.Parsec                  hiding (string)
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Control.Lens

import           Data.Tuple.Strict
import qualified Foreign.Ruby.Helpers         as FR
import qualified Foreign.Ruby.Bindings        as FR
import           Foreign.Ruby
import           GHC.Conc (labelThread)

import           Puppet.Interpreter.Types
import           Puppet.Interpreter.IO
import           Puppet.Interpreter.Resolve
import           Puppet.PP
import           Puppet.Preferences
import           Puppet.Stats
import           Puppet.Utils

instance IsString TemplateParseError where
  fromString s = TemplateParseError $ newErrorMessage (Message s) (initialPos "dummy")

newtype TemplateParseError = TemplateParseError { tgetError :: ParseError }

type TemplateQuery = (Chan TemplateAnswer, Either T.Text T.Text, InterpreterState, InterpreterReader IO)
type TemplateAnswer = S.Either PrettyError T.Text

showRubyError :: RubyError -> PrettyError
showRubyError (Stack msg stk) = PrettyError $ dullred (string msg) </> dullyellow (string stk)
showRubyError (WithOutput str _) = PrettyError $ dullred (string str)
showRubyError (OtherError rr) = PrettyError (dullred (text rr))

initTemplateDaemon :: RubyInterpreter -> Preferences IO -> MStats -> IO (Either T.Text T.Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError T.Text))
initTemplateDaemon intr prefs mvstats = do
    controlchan <- newChan
    templatecache <- newFileCache
    let returnError rs = return $ \_ _ _ -> return (S.Left (showRubyError rs))
    x <- runExceptT $ do
        liftIO (getRubyScriptPath "hrubyerb.rb") >>= ExceptT . loadFile intr
        ExceptT (registerGlobalFunction4 intr "varlookup" hrresolveVariable)
        ExceptT (registerGlobalFunction5 intr "callextfunc" hrcallfunction)
        liftIO $ void $ forkIO $ templateDaemon intr
                                                (T.pack (prefs ^. prefPuppetPaths.modulesPath))
                                                (T.pack (prefs ^. prefPuppetPaths.templatesPath))
                                                controlchan
                                                mvstats
                                                templatecache
        return (templateQuery controlchan)
    either returnError return x

templateQuery :: Chan TemplateQuery -> Either T.Text T.Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError T.Text)
templateQuery qchan filename stt rdr = do
    rchan <- newChan
    writeChan qchan (rchan, filename, stt, rdr)
    readChan rchan

templateDaemon :: RubyInterpreter -> T.Text -> T.Text -> Chan TemplateQuery -> MStats -> FileCacheR TemplateParseError [RubyStatement] -> IO ()
templateDaemon intr modpath templatepath qchan mvstats filecache = do
    let nameThread :: String -> IO ()
        nameThread n = myThreadId >>= flip labelThread n
    nameThread "RubyTemplateDaemon"

    (respchan, fileinfo, stt, rdr) <- readChan qchan
    case fileinfo of
        Right filename -> do
            let prts = T.splitOn "/" filename
                searchpathes | length prts > 1 = [modpath <> "/" <> head prts <> "/templates/" <> T.intercalate "/" (tail prts), templatepath <> "/" <> filename]
                             | otherwise        = [templatepath <> "/" <> filename]
            acceptablefiles <- filterM (fileExist . T.unpack) searchpathes
            if null acceptablefiles
                then writeChan respchan (S.Left $ PrettyError $ "Can't find template file for" <+> ttext filename <+> ", looked in" <+> list (map ttext searchpathes))
                else measure mvstats filename (computeTemplate intr (Right (head acceptablefiles)) stt rdr mvstats filecache) >>= writeChan respchan
        Left _ -> measure mvstats "inline" (computeTemplate intr fileinfo stt rdr mvstats filecache) >>= writeChan respchan
    templateDaemon intr modpath templatepath qchan mvstats filecache

computeTemplate :: RubyInterpreter -> Either T.Text T.Text -> InterpreterState -> InterpreterReader IO -> MStats -> FileCacheR TemplateParseError [RubyStatement] -> IO TemplateAnswer
computeTemplate intr fileinfo stt rdr mstats filecache = do
    let (curcontext, fvariables) = case extractFromState stt of
                                       Nothing -> (mempty, mempty)
                                       Just (c,v) -> (c,v)
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
            measure mstats ("ruby - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables stt rdr
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (S.Right ev)
                Left err -> do
                    let !msg = "template " ++ ufilename ++ " evaluation failed " ++ show err
                    traceEventIO msg
                    LOG.debugM "Erb.Compute" msg
                    measure mstats ("ruby efail - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables stt rdr
    traceEventIO ("STOP template " ++ T.unpack filename)
    return o

getRubyScriptPath :: String -> IO String
getRubyScriptPath rubybin = do
    let checkpath :: FilePath -> IO FilePath -> IO FilePath
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
                     Right "~g~e~t_h~a~s~h~" ->
                        let getvars ctx = (variables ^. ix ctx . scopeVariables) & traverse %~ view (_1 . _1)
                            vars = getvars "::" <> getvars scope
                        in  Right (PHash vars)
                     Right t -> getVariable variables scope t
                     Left rr -> Left ("The variable name is not a string" <+> text rr)
    case answer of
        Left _  -> getSymbol "undef"
        Right r -> FR.toRuby r

hrcallfunction :: RValue -> RValue -> RValue -> RValue -> RValue -> IO RValue
hrcallfunction _ rfname rargs rstt rrdr = do
    efname <- FR.fromRuby rfname
    eargs <- FR.fromRuby rargs
    rdr <- FR.extractHaskellValue rrdr
    stt <- FR.extractHaskellValue rstt
    let err :: String -> IO RValue
        err rr = fmap (either Prelude.snd id) (FR.toRuby (T.pack rr) >>= FR.safeMethodCall "MyError" "new" . (:[]))
    case (,) <$> efname <*> eargs of
        Right (fname, varray) -> do
            let args = case varray of
                           [PArray vargs] -> V.toList vargs
                           _ -> varray
            (x,_,_) <- interpretMonad rdr stt (resolveFunction' fname args)
            case x of
                Right o -> case o ^? _Number of
                              Just n -> FR.toRuby n
                              Nothing -> FR.toRuby o
                Left rr -> err (show rr)
        Left rr -> err rr

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> InterpreterState -> InterpreterReader IO -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables stt rdr = FR.freezeGC $ eitherDocIO $ do
    rscp <- FR.embedHaskellValue curcontext
    rvariables <- FR.embedHaskellValue variables
    rstt <- FR.embedHaskellValue stt
    rrdr <- FR.embedHaskellValue rdr
    let varlist = variables ^. ix curcontext . scopeVariables
    -- must be called from a "makeSafe" thingie
    contentinfo <- case fileinfo of
                       Right fname -> FR.toRuby fname
                       Left _ -> FR.toRuby ("-" :: T.Text)
    let withBinding f = do
            erbBinding <- FR.safeMethodCall "ErbBinding" "new" [rscp,rvariables,rstt,rrdr,contentinfo]
            case erbBinding of
                Left x -> return (Left x)
                Right v -> do
                     forM_ (itoList varlist) $ \(varname, varval :!: _ :!: _) -> FR.toRuby varval >>= FR.rb_iv_set v (T.unpack varname)
                     f v
    o <- case fileinfo of
             Right fname  -> do
                 rfname <- FR.toRuby fname
                 withBinding $ \v -> FR.safeMethodCall "Controller" "runFromFile" [rfname,v]
             Left content -> withBinding $ \v -> FR.toRuby content >>= FR.safeMethodCall "Controller" "runFromContent" . (:[v])
    FR.freeHaskellValue rrdr
    FR.freeHaskellValue rstt
    FR.freeHaskellValue rvariables
    FR.freeHaskellValue rscp
    case o of
        Left (rr, _) ->
            let fname = case fileinfo of
                            Right f -> T.unpack f
                            Left _  -> "inline_template"
            in  return (S.Left $ PrettyError (dullred (text rr) <+> "in" <+> dullgreen (text fname)))
        Right r -> FR.fromRuby r >>= \case
            Right result -> return (S.Right result)
            Left rr -> return (S.Left $ PrettyError ("Could not deserialiaze ruby output" <+> text rr))

eitherDocIO :: IO (S.Either PrettyError a) -> IO (S.Either PrettyError a)
eitherDocIO computation = (computation >>= check) `catch` (\e -> return $ S.Left $ PrettyError $ dullred $ text $ show (e :: SomeException))
    where
        check (S.Left r) = return (S.Left r)
        check (S.Right x) = return (S.Right x)
