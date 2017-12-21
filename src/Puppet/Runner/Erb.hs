{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Internal module used to initialize the erb template daemon.
module Puppet.Runner.Erb (
     initTemplateDaemon
   , rubyEvaluate
) where

import           XPrelude -- hiding (option, try)

import           Data.Aeson.Lens            (_Number)
import qualified Data.Either.Strict         as S
import qualified Data.FileCache             as Cache
import qualified Data.List                  as List
import qualified Data.Text                  as Text
import           Data.Tuple.Strict          (Pair (..))
import qualified Data.Vector                as V
import           Debug.Trace
import           Foreign.Ruby
import qualified Foreign.Ruby.Bindings      as FR
import qualified Foreign.Ruby.Helpers       as FR
import           GHC.Conc                   (labelThread)
import           Paths_language_puppet      (getDataFileName)
import           System.Environment         (getExecutablePath)
import           System.FilePath
import           System.Posix.Files
import           Text.Parsec                hiding (string)
import           Text.Parsec.Error
import           Text.Parsec.Pos

import           Erb
import           Puppet.Runner.Erb.Evaluate
import           Puppet.Interpreter.Helpers
import           Puppet.Interpreter.IO
import           Puppet.Interpreter.Resolve
import           Puppet.Interpreter.Types
import           Puppet.Runner.Preferences
import           Puppet.Runner.Stats

instance IsString TemplateParseError where
  fromString s = TemplateParseError $ newErrorMessage (Message s) (initialPos "dummy")

newtype TemplateParseError = TemplateParseError { tgetError :: ParseError }

type TemplateQuery = (Chan TemplateAnswer, Either Text Text, InterpreterState, InterpreterReader IO)
type TemplateAnswer = S.Either PrettyError Text

showRubyError :: RubyError -> PrettyError
showRubyError (Stack msg stk) = PrettyError $ dullred (ppstring msg) <> softline <> dullyellow (ppstring stk)
showRubyError (WithOutput str _) = PrettyError $ dullred (ppstring str)
showRubyError (OtherError rr) = PrettyError (dullred (pptext rr))

-- | Parse and resolve erb files. Initializes a thread for the Ruby interpreter .
initTemplateDaemon :: RubyInterpreter -> Preferences IO -> MStats -> IO (Either Text Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError Text))
initTemplateDaemon intr prefs mvstats = do
    controlchan <- newChan
    templatecache <- Cache.newFileCache
    let returnError rs = return $ \_ _ _ -> return (S.Left (showRubyError rs))
    x <- runExceptT $ do
        liftIO (getRubyScriptPath "hrubyerb.rb") >>= ExceptT . loadFile intr
        ExceptT (registerGlobalFunction4 intr "varlookup" hrresolveVariable)
        ExceptT (registerGlobalFunction5 intr "callextfunc" hrcallfunction)
        liftIO $ void $ forkIO $ templateDaemon intr
                                                (Text.pack (prefs ^. prefPuppetPaths.modulesPath))
                                                (Text.pack (prefs ^. prefPuppetPaths.templatesPath))
                                                controlchan
                                                mvstats
                                                templatecache
        pure $! templateQuery controlchan
    either returnError return x

templateQuery :: Chan TemplateQuery -> Either Text Text -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError Text)
templateQuery qchan filename stt rdr = do
    rchan <- newChan
    writeChan qchan (rchan, filename, stt, rdr)
    readChan rchan

templateDaemon :: RubyInterpreter -> Text -> Text -> Chan TemplateQuery -> MStats -> Cache.FileCacheR TemplateParseError [RubyStatement] -> IO ()
templateDaemon intr modpath templatepath qchan mvstats filecache = do
    let nameThread :: String -> IO ()
        nameThread n = myThreadId >>= flip labelThread n
    nameThread "RubyTemplateDaemon"

    (respchan, fileinfo, stt, rdr) <- readChan qchan
    case fileinfo of
        Right filename -> do
            let prts = Text.splitOn "/" filename
                searchpathes | length prts > 1 = [modpath <> "/" <> List.head prts <> "/templates/" <> Text.intercalate "/" (List.tail prts), templatepath <> "/" <> filename]
                             | otherwise        = [templatepath <> "/" <> filename]
            acceptablefiles <- filterM (fileExist . Text.unpack) searchpathes
            if null acceptablefiles
                then writeChan respchan (S.Left $ PrettyError $ "Can't find template file for" <+> ppline filename <+> ", looked in" <+> list (map ppline searchpathes))
                else measure mvstats filename (computeTemplate intr (Right (List.head acceptablefiles)) stt rdr mvstats filecache) >>= writeChan respchan
        Left _ -> measure mvstats "inline" (computeTemplate intr fileinfo stt rdr mvstats filecache) >>= writeChan respchan
    templateDaemon intr modpath templatepath qchan mvstats filecache

computeTemplate :: RubyInterpreter -> Either Text Text -> InterpreterState -> InterpreterReader IO -> MStats -> Cache.FileCacheR TemplateParseError [RubyStatement] -> IO TemplateAnswer
computeTemplate intr fileinfo stt rdr mstats filecache = do
    let (curcontext, fvariables) = case extractFromState stt of
          Nothing    -> (mempty, mempty)
          Just (c,v) -> (c,v)
    let (filename, ufilename) = case fileinfo of
          Left _  -> ("inline", "inline")
          Right x -> (x, Text.unpack x)
        mkSafe a = makeSafe intr a >>= \case
          Left rr -> return (S.Left (showRubyError rr))
          Right x -> return x
        encapsulateError = _Left %~ TemplateParseError
        variables = fvariables & traverse . scopeVariables . traverse . _1 . _1 %~ toStr
        toStr (PNumber n) = PString (scientific2text n)
        toStr x           = x
    traceEventIO ("START template " ++ Text.unpack filename)
    parsed <- case fileinfo of
                  Right _      -> measure mstats ("parsing - " <> filename) $ Cache.lazyQuery filecache ufilename $ fmap encapsulateError (parseErbFile ufilename)
                  Left content -> measure mstats ("parsing - " <> filename) $ return $ encapsulateError (runParser erbparser () "inline" (Text.unpack content))
    o <- case parsed of
        Left err -> do
            let msg = "Template '" <> toS ufilename <> "' could not be parsed " <> show (tgetError err)
            logDebug msg
            measure mstats ("ruby - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables stt rdr
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (S.Right ev)
                Left err -> do
                    let !msg = "Template '" <> toS ufilename <> "' evaluation failed with: " <> show err
                    logDebug msg
                    measure mstats ("ruby efail - " <> filename) $ mkSafe $ computeTemplateWRuby fileinfo curcontext variables stt rdr
    traceEventIO ("STOP template " ++ Text.unpack filename)
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
            path <- fmap takeDirectory getExecutablePath
            let fullpath = path </> rubybin
            checkpath fullpath $ checkpath ("/usr/local/bin/" <> rubybin) (return rubybin)
    cabalPath <- getDataFileName $ "ruby/" ++ rubybin :: IO FilePath
    checkpath cabalPath withExecutablePath

-- This must be called from the proper thread. As this is callback, this
-- should be ok.
hrresolveVariable :: RValue -> RValue -> RValue -> RValue -> IO RValue
-- Text -> Container PValue -> RValue -> RValue -> IO RValue
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
                     Left rr -> Left ("The variable name is not a string" <+> pptext rr)
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
        err rr = fmap (either snd identity) (FR.toRuby (Text.pack rr) >>= FR.safeMethodCall "MyError" "new" . (:[]))
    case (,) <$> efname <*> eargs of
        Right (fname, varray) | fname `elem` ["template", "inline_template"] -> do
          logError $ "Can't parse a call to the external ruby function '" <> fname <> "'  n an erb file.\n\tIt is not possible to call it from a Ruby function. It would stall (yes it sucks ...).\n\tChoosing to output \"undef\" !"
          getSymbol "undef"
                              | otherwise -> do
          let args = case varray of
                         [PArray vargs] -> V.toList vargs
                         _              -> varray
          (x,_,_) <- interpretMonad rdr stt (resolveFunction' fname args)
          case x of
              Right o -> case o ^? _Number of
                            Just n  -> FR.toRuby n
                            Nothing -> FR.toRuby o
              Left rr -> err (show rr)
        Left rr -> err rr

computeTemplateWRuby :: Either Text Text -> Text -> Container ScopeInformation -> InterpreterState -> InterpreterReader IO -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables stt rdr = FR.freezeGC $ eitherDocIO $ do
    rscp <- FR.embedHaskellValue curcontext
    rvariables <- FR.embedHaskellValue variables
    rstt <- FR.embedHaskellValue stt
    rrdr <- FR.embedHaskellValue rdr
    let varlist = variables ^. ix curcontext . scopeVariables
    -- must be called from a "makeSafe" thingie
    contentinfo <- case fileinfo of
                       Right fname -> FR.toRuby fname
                       Left _      -> FR.toRuby ("-" :: Text)
    let withBinding f = do
            erbBinding <- FR.safeMethodCall "ErbBinding" "new" [rscp,rvariables,rstt,rrdr,contentinfo]
            case erbBinding of
                Left x -> return (Left x)
                Right v -> do
                     forM_ (itoList varlist) $ \(varname, varval :!: _ :!: _) -> FR.toRuby varval >>= FR.rb_iv_set v (Text.unpack varname)
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
                            Right f -> Text.unpack f
                            Left _  -> "inline_template"
            in  return (S.Left $ PrettyError (dullred (pptext rr) <+> "in" <+> dullgreen (pptext fname)))
        Right r -> FR.fromRuby r >>= \case
            Right result -> return (S.Right result)
            Left rr -> return (S.Left $ PrettyError ("Could not deserialiaze ruby output" <+> pptext rr))

eitherDocIO :: IO (S.Either PrettyError a) -> IO (S.Either PrettyError a)
eitherDocIO computation = (computation >>= check) `catch` (\e -> return $ S.Left $ PrettyError $ dullred $ ppline $ show (e :: SomeException))
    where
        check (S.Left r)  = return (S.Left r)
        check (S.Right x) = return (S.Right x)
