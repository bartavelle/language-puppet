{-# LANGUAGE NamedFieldPuns #-}
-- | Internal module used to initialize the erb template daemon.
module Puppet.Runner.Erb (
     initTemplateDaemon
   , rubyEvaluate
) where

import           XPrelude

import qualified Data.Either.Strict         as S
import qualified Data.FileCache             as Cache
import qualified Data.List                  as List
import qualified Data.List.Split            as List
import qualified Data.Text                  as Text
import           Data.Tuple.Strict          (Pair (..))
import qualified Data.Vector                as Vector
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
import           Puppet.Interpreter.Helpers
import           Puppet.Interpreter.IO
import           Puppet.Interpreter.Resolve
import           Puppet.Interpreter.Types
import           Puppet.Runner.Erb.Evaluate
import           Puppet.Runner.Preferences
import           Puppet.Runner.Stats


instance IsString TemplateParseError where
  fromString s = TemplateParseError $ newErrorMessage (Message s) (initialPos mempty)

newtype TemplateParseError = TemplateParseError { tgetError :: ParseError }

type TemplateQuery = (Chan TemplateAnswer, TemplateSource, InterpreterState, InterpreterReader IO)
type TemplateAnswer = S.Either PrettyError Text

showRubyError :: RubyError -> PrettyError
showRubyError (Stack msg stk) = PrettyError $ dullred (ppstring msg) <> softline <> dullyellow (ppstring stk)
showRubyError (WithOutput str _) = PrettyError $ dullred (ppstring str)
showRubyError (OtherError rr) = PrettyError (dullred (pptext rr))

-- | Parse and resolve erb files. Initializes a thread for the Ruby interpreter .
initTemplateDaemon :: RubyInterpreter
                   -> Preferences IO
                   -> MStats
                   -> IO (TemplateSource -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError Text))
initTemplateDaemon rubyintp prefs mvstats = do
  controlchan <- newChan
  templatecache <- Cache.newFileCache
  let returnError rs = return $ \_ _ _ -> pure (S.Left (showRubyError rs))
  x <- runExceptT $ do
    liftIO (getRubyScriptPath "hrubyerb.rb") >>= ExceptT . loadFile rubyintp
    ExceptT (registerGlobalFunction4 rubyintp "varlookup" hrresolveVariable)
    ExceptT (registerGlobalFunction5 rubyintp "callextfunc" hrcallfunction)
    liftIO $ void $ forkIO $ templateDaemon rubyintp
                                            (prefs ^. prefPuppetPaths.modulesPath)
                                            (prefs ^. prefPuppetPaths.templatesPath)
                                            controlchan
                                            mvstats
                                            templatecache
    pure $! templateQuery controlchan
  either returnError pure x

templateQuery :: Chan TemplateQuery -> TemplateSource -> InterpreterState -> InterpreterReader IO -> IO (S.Either PrettyError Text)
templateQuery qchan filename intpstate intpreader = do
  rchan <- newChan
  writeChan qchan (rchan, filename, intpstate, intpreader)
  readChan rchan

templateDaemon :: RubyInterpreter -> FilePath -> FilePath -> Chan TemplateQuery -> MStats -> Cache.FileCacheR TemplateParseError [RubyStatement] -> IO ()
templateDaemon rubyintp modpath templatepath qchan mvstats filecache = do
  let nameThread :: String -> IO ()
      nameThread n = myThreadId >>= flip labelThread n
  nameThread "RubyTemplateDaemon"
  (respchan, fileinfo, intpstate, intpreader) <- readChan qchan
  case fileinfo of
    Filename filename -> do
      let prts = List.splitOn "/" filename
          searchpathes | length prts > 1 = [ modpath </> List.head prts </> "templates" </> List.intercalate "/" (List.tail prts)
                                           , templatepath </> filename
                                           ]
                       | otherwise        = [templatepath </> filename]
      acceptablefiles <- filterM fileExist searchpathes
      if null acceptablefiles
        then writeChan respchan (S.Left $ PrettyError $ "Can't find template file for" <+> pptext filename <+> ", looked in" <+> list (map pptext searchpathes))
        else measure mvstats (toS filename) (computeTemplate rubyintp (Filename (List.head acceptablefiles)) intpstate intpreader mvstats filecache) >>= writeChan respchan
    Inline _ -> measure mvstats "inline" (computeTemplate rubyintp fileinfo intpstate intpreader mvstats filecache) >>= writeChan respchan
  templateDaemon rubyintp modpath templatepath qchan mvstats filecache

computeTemplate :: RubyInterpreter -> TemplateSource -> InterpreterState -> InterpreterReader IO -> MStats -> Cache.FileCacheR TemplateParseError [RubyStatement] -> IO TemplateAnswer
computeTemplate rubyintp srcinfo intpstate intpreader mstats filecache = do
  let (curcontext, fvariables) =
        case extractScope intpstate of
          Nothing    -> (mempty, mempty)
          Just (c,v) -> (c,v)
      template_src = templateSrc srcinfo
      mkSafe a = makeSafe rubyintp a >>= \case
        Left err -> return (S.Left (showRubyError err))
        Right x -> return x
      encapsulateError = _Left %~ TemplateParseError
      variables = fvariables & traverse . scopeVariables . traverse . _1 . _1 %~ toStr
      toStr (PNumber n) = PString (scientific2text n)
      toStr x           = x
  traceEventIO ("START template " <> template_src)
  parsed <- case srcinfo of
    Filename _ -> measure mstats ("parsing - " <> toS template_src) $ Cache.lazyQuery filecache template_src $ fmap encapsulateError (parseErbFile template_src)
    Inline s -> measure mstats ("parsing - " <> toS template_src) $ pure $ encapsulateError (runParser erbparser () "inline" (toS s))
  o <- case parsed of
    Left err -> do
      let !msg = "Template could not be parsed " <> show (tgetError err)
      -- if the haskell parser fails the ruby one will fallback.
      logInfoStr msg
      measure mstats ("ruby - " <> toS template_src) $ mkSafe $ computeTemplateWRuby srcinfo curcontext variables intpstate intpreader
    Right ast -> case rubyEvaluate variables curcontext ast of
      Right ev -> pure (S.Right ev)
      Left err -> do
        let !msg = "At " <> showPPos'(intpstate^.curPos) <> " the evaluation of template '" <> template_src <> "' failed. " <> show err
      -- if the haskell evaluation fails the ruby one will fallback. It is likely that the reason for the failure is a real template issue.
        logErrorStr msg
        measure mstats ("ruby efail - " <> toS template_src) $ mkSafe $ computeTemplateWRuby srcinfo curcontext variables intpstate intpreader
  traceEventIO ("STOP template " <> template_src)
  pure o

getRubyScriptPath :: String -> IO String
getRubyScriptPath rubybin = do
  let checkpath :: FilePath -> IO FilePath -> IO FilePath
      checkpath fp nxt =
        ifM (fileExist fp)
          (pure fp)
          nxt
      withExecutablePath = do
        path <- fmap takeDirectory getExecutablePath
        let fullpath = path </> rubybin
        checkpath fullpath $ checkpath ("/usr/local/bin/" <> rubybin) (pure rubybin)
  cabalPath <- getDataFileName $ "ruby/" ++ rubybin :: IO FilePath
  checkpath cabalPath withExecutablePath

-- This must be called from the proper thread. As this is callback, this should be ok.
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
        Left err -> Left ("The variable name is not a string" <+> pptext err)
  case answer of
    Left _  -> getSymbol "undef"
    Right r -> FR.toRuby r

hrcallfunction :: RValue -> RValue -> RValue -> RValue -> RValue -> IO RValue
hrcallfunction _ rfname rargs rstt rrdr = do
  efname <- FR.fromRuby rfname
  eargs <- FR.fromRuby rargs
  rdr <- FR.extractHaskellValue rrdr
  stt <- FR.extractHaskellValue rstt
  let rubyerr :: String -> IO RValue
      rubyerr err = fmap (either snd identity) (FR.toRuby (Text.pack err) >>= FR.safeMethodCall "MyError" "new" . (:[]))
  case (,) <$> efname <*> eargs of
    Right (fname, varray) | fname `elem` ["template", "inline_template"] -> do
      logWarning $ "Can't parse a call to the external ruby function '" <> fname <> "'  n an erb file.\n\tIt is not possible to call it from a Ruby function. It would stall (yes it sucks ...).\n\tChoosing to output \"undef\" !"
      getSymbol "undef"
                          | otherwise -> do
      let args = case varray of
            [PArray vargs] -> Vector.toList vargs
            _              -> varray
      (x,_,_) <- interpretMonad rdr stt (resolveFunction' fname args)
      case x of
        Right o -> case o ^? _PValueNumber of
          Just n  -> FR.toRuby n
          Nothing -> FR.toRuby o
        Left err -> rubyerr (show err)
    Left err -> rubyerr err

computeTemplateWRuby :: TemplateSource -> Text -> Container ScopeInformation -> InterpreterState -> InterpreterReader IO -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables stt rdr = FR.freezeGC $ eitherDocIO $ do
  rscp <- FR.embedHaskellValue curcontext
  rvariables <- FR.embedHaskellValue variables
  rstt <- FR.embedHaskellValue stt
  rrdr <- FR.embedHaskellValue rdr
  let varlist = variables ^. ix curcontext . scopeVariables
  -- must be called from a "makeSafe" thingie
  contentinfo <- case fileinfo of
    Filename fname -> FR.toRuby (Text.pack fname)
    Inline _      -> FR.toRuby ("-" :: Text)
  let withBinding f =
        FR.safeMethodCall "ErbBinding" "new" [rscp,rvariables,rstt,rrdr,contentinfo] >>= \case
          Left x -> pure (Left x)
          Right v -> do
           forM_ (itoList varlist) $ \(varname, (varval :!: _) :!: _) -> FR.toRuby varval >>= FR.rb_iv_set v (toS varname)
           f v
  o <- case fileinfo of
    Filename fname  -> do
      rfname <- FR.toRuby (Text.pack fname)
      withBinding $ \v -> FR.safeMethodCall "Controller" "runFromFile" [rfname,v]
    Inline content -> withBinding $ \v -> FR.toRuby content >>= FR.safeMethodCall "Controller" "runFromContent" . (:[v])
  FR.freeHaskellValue rrdr
  FR.freeHaskellValue rstt
  FR.freeHaskellValue rvariables
  FR.freeHaskellValue rscp
  case o of
    Left (err, _) ->
      pure (S.Left $ PrettyError (dullred (pptext err) <+> "in" <+> dullgreen (pptext (templateSrc fileinfo))))
    Right r -> FR.fromRuby r >>= \case
      Right result -> pure (S.Right result)
      Left err -> pure (S.Left $ PrettyError ("Could not deserialiaze ruby output" <+> pptext err))

eitherDocIO :: IO (S.Either PrettyError a) -> IO (S.Either PrettyError a)
eitherDocIO computation =
  (computation >>= check) `catch` (\e -> pure $ S.Left $ PrettyError $ dullred $ ppline $ show (e :: SomeException))
  where
    check (S.Left r)  = pure (S.Left r)
    check (S.Right x) = pure (S.Right x)

templateSrc :: TemplateSource -> String
templateSrc = \case
  Inline _ -> "inline_template"
  Filename n -> n
