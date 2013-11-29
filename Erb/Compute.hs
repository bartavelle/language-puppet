{-# LANGUAGE CPP #-}
#ifdef HRUBY
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
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
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import System.Environment
import Data.FileCache

#ifdef HRUBY
import Foreign hiding (void)
import Foreign.Ruby

import Control.Lens
import Data.Tuple.Strict
type RegisteredGetvariable = RValue -> RValue -> RValue -> RValue -> IO RValue
foreign import ccall "wrapper" mkRegisteredGetvariable :: RegisteredGetvariable -> IO (FunPtr RegisteredGetvariable)

#else
import System.IO
import SafeProcess
import Data.List
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Foldable as F
import qualified Data.Vector as V
#endif

instance Error ParseError where
    noMsg = newErrorUnknown (initialPos "dummy")
    strMsg s = newErrorMessage (Message s) (initialPos "dummy")

type TemplateQuery = (Chan TemplateAnswer, Either T.Text T.Text, T.Text, Container ScopeInformation)
type TemplateAnswer = S.Either Doc T.Text

initTemplateDaemon :: Preferences -> MStats -> IO (Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text))
initTemplateDaemon (Preferences _ modpath templatepath _ _ _ _ _ _) mvstats = do
    controlchan <- newChan
    templatecache <- newFileCache
#ifdef HRUBY
    -- forkOS is used because ruby doesn't like to change threads
    -- all initialization is done on the current thread
    void $ forkOS $ do
        initialize
        s <- (getRubyScriptPath "hrubyerb.rb" >>= \p -> rb_load_protect p 0)
        unless (s == 0) $ do
            msg <- showErrorStack
            error ("initTemplateDaemon: " ++ msg)
        rubyResolveFunction <- mkRegisteredGetvariable hrresolveVariable
        rb_define_global_function "varlookup" rubyResolveFunction 3
        templateDaemon (T.pack modpath) (T.pack templatepath) controlchan mvstats templatecache
#else
    forkIO (templateDaemon (T.pack modpath) (T.pack templatepath) controlchan mvstats templatecache)
#endif
    return (templateQuery controlchan)

templateQuery :: Chan TemplateQuery -> Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text)
templateQuery qchan filename scope variables = do
    rchan <- newChan
    writeChan qchan (rchan, filename, scope, variables)
    readChan rchan

templateDaemon :: T.Text -> T.Text -> Chan TemplateQuery -> MStats -> FileCacheR ParseError [RubyStatement] -> IO ()
templateDaemon modpath templatepath qchan mvstats filecache = do
    (respchan, fileinfo, scope, variables) <- readChan qchan
    case fileinfo of
        Right filename -> do
            let prts = T.splitOn "/" filename
                searchpathes | length prts > 1 = [modpath <> "/" <> head prts <> "/templates/" <> T.intercalate "/" (tail prts), templatepath <> "/" <> filename]
                             | otherwise        = [templatepath <> "/" <> filename]
            acceptablefiles <- filterM (fileExist . T.unpack) searchpathes
            if null acceptablefiles
                then writeChan respchan (S.Left $ "Can't find template file for" <+> ttext filename <+> ", looked in" <+> list (map ttext searchpathes))
                else measure mvstats ("total - " <> filename) (computeTemplate (Right (head acceptablefiles)) scope variables mvstats filecache) >>= writeChan respchan
        Left _ -> measure mvstats "total - inline" (computeTemplate fileinfo scope variables mvstats filecache) >>= writeChan respchan
    templateDaemon modpath templatepath qchan mvstats filecache

computeTemplate :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> MStats -> FileCacheR ParseError [RubyStatement] -> IO TemplateAnswer
computeTemplate fileinfo curcontext variables mstats filecache = do
    let (filename, ufilename) = case fileinfo of
                                    Left _ -> ("inline", "inline")
                                    Right x -> (x, T.unpack x)
    parsed <- case fileinfo of
                  Right _      -> measure mstats ("parsing - " <> filename) $ lazyQuery filecache ufilename $ parseErbFile ufilename
                  Left content -> measure mstats ("parsing - " <> filename) $ return (runParser erbparser () "inline" (T.unpack content))
    case parsed of
        Left err -> do
            let !msg = "template " ++ ufilename ++ " could not be parsed " ++ show err
            traceEventIO msg
            LOG.debugM "Erb.Compute" msg
            measure mstats ("ruby - " <> filename) $ computeTemplateWRuby fileinfo curcontext variables
        Right ast -> case rubyEvaluate variables curcontext ast of
                Right ev -> return (S.Right ev)
                Left err -> do
                    let !msg = "template " ++ ufilename ++ " evaluation failed " ++ show err
                    traceEventIO msg
                    LOG.debugM "Erb.Compute" msg
                    measure mstats ("ruby efail - " <> filename) $ computeTemplateWRuby fileinfo curcontext variables

getRubyScriptPath :: String -> IO String
getRubyScriptPath rubybin = do
    cabalPath <- getDataFileName $ "ruby/" ++ rubybin :: IO FilePath
    exists    <- fileExist cabalPath
    if exists
        then return cabalPath
        else do
            path <- fmap (T.unpack . takeDirectory . T.pack) getExecutablePath
            let fullpath = path <> "/" <> rubybin
            lexists <- fileExist cabalPath
            return $ if lexists
                         then fullpath
                         else rubybin

#ifdef HRUBY
hrresolveVariable :: RValue -> RValue -> RValue -> RValue -> IO RValue
-- T.Text -> Container PValue -> RValue -> RValue -> IO RValue
hrresolveVariable _ rscp rvariables rtoresolve = do
    scope <- extractHaskellValue rscp
    variables <- extractHaskellValue rvariables
    toresolve <- fromRuby rtoresolve
    let answer = case toresolve of
                     Just t -> getVariable variables scope t
                     _ -> Left "The variable name is not a string"
    case answer of
        Left _  -> getSymbol "undef"
        Right r -> toRuby r

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables = freezeGC $ eitherDocIO $ do
    rscp <- embedHaskellValue curcontext
    rvariables <- embedHaskellValue variables
    let varlist = variables ^. ix curcontext . scopeVariables
    let withBinding f = do
            erbBinding <- safeMethodCall "ErbBinding" "new" [rscp,rvariables]
            case erbBinding of
                Left x -> return (Left x)
                Right v -> do
                     forM_ (itoList varlist) $ \(varname, (varval :!: _ :!: _)) -> toRuby varval >>= rb_iv_set v (T.unpack varname)
                     f v
    o <- case fileinfo of
             Right fname  -> do
                 rfname <- toRuby fname
                 withBinding $ \v -> safeMethodCall "Controller" "runFromFile" [rfname,v]
             Left content -> withBinding $ \v -> toRuby content >>= safeMethodCall "Controller" "runFromContent" . (:[v])
    freeHaskellValue rvariables
    freeHaskellValue rscp
    case o of
        Left (rr, _) ->
            let fname = case fileinfo of
                            Right f -> T.unpack f
                            Left _  -> "inline_template"
            in  return (S.Left (dullred (text rr) <+> "in" <+> dullgreen (text fname)))
        Right r -> fromRuby r >>= \case
                                    Just result -> return (S.Right result)
                                    Nothing -> return (S.Left "Could not deserialiaze ruby output")

#else
saveTmpContent :: T.Text -> IO FilePath
saveTmpContent cnt = do
    (name, h) <- openTempFile "/tmp" "inline_template.erb"
    T.hPutStr h cnt
    hClose h
    return name

computeTemplateWRuby :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO TemplateAnswer
computeTemplateWRuby fileinfo curcontext variables = do
    (temp, filename) <- case fileinfo of
                            Right x  -> return (Nothing, x)
                            Left cnt -> do
                                tmpfile <- saveTmpContent cnt
                                return (Just tmpfile, "inline")
    let rubyvars = "{\n" <> mconcat (intersperse ",\n" (concatMap toRubyVars (itoList variables))) <> "\n}\n" :: T.Builder
        input = T.fromText curcontext <> "\n" <> T.fromText filename <> "\n" <> rubyvars :: T.Builder
        ufilename = T.unpack filename
    rubyscriptpath <- getRubyScriptPath "calcerb.rb"
    traceEventIO ("start running ruby" ++ ufilename)
    !ret <- safeReadProcessTimeout "ruby" [rubyscriptpath] (T.toLazyText input) 1000
    traceEventIO ("finished running ruby" ++ ufilename)
    F.forM_ temp removeLink
    case ret of
        Just (Right x) -> return $! S.Right x
        Just (Left er) -> do
            (tmpfilename, tmphandle) <- openTempFile "/tmp" "templatefail"
            TL.hPutStr tmphandle (T.toLazyText input)
            hClose tmphandle
            return $ S.Left $ dullred (text er) <+> "- for template" <+> text ufilename <+> "input in" <+> text tmpfilename
        Nothing -> return $ S.Left "Process did not terminate"

minterc :: T.Builder -> [T.Builder] -> T.Builder
minterc _ [] = mempty
minterc _ [a] = a
minterc !separator (x:xs) = x <> foldl' minterc' mempty xs
    where
        minterc' !curbuilder !b  = curbuilder <> separator <> b

renderString :: T.Text -> T.Builder
renderString x = let !y = T.fromString (show x) in y

toRubyVars :: (T.Text, ScopeInformation) -> [T.Builder]
toRubyVars (ctx, scp) = concatMap (\(varname, varval :!: _) -> toRuby (ctx <> "::" <> varname, varval)) (itoList (scp ^. scopeVariables))

toRuby :: (T.Text, PValue) -> [T.Builder]
toRuby (_, PUndef) = []
toRuby (varname, varval) = ["\t" <> renderString varname <> " => " <> toRuby' varval]

toRuby' :: PValue -> T.Builder
toRuby' (PString str) = renderString str
toRuby' (PBoolean True) = "true"
toRuby' (PBoolean False) = "false"
toRuby' (PArray rr) = "[" <> minterc ", " (map toRuby' (rr ^.. traverse)) <> "]"
toRuby' (PHash hh) = "{ " <> minterc ", " (map (\(varname, varval) -> renderString varname <> " => " <> toRuby' varval) (itolist hh)) <>  " }"
toRuby' PUndef = ":undef"
toRuby' (PResourceReference rtype rname) = renderString ( rtype <> "[" <> rname <> "]" )
#endif
