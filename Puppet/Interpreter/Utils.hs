{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
module Puppet.Interpreter.Utils where

import           Control.Exception
import           Control.Monad.Operational
import           Data.Maybe                  (fromMaybe)
import           Control.Monad.Writer.Class
import qualified Data.Text.Encoding          as T
import qualified Data.Text                   as T
import qualified Data.Either.Strict          as S
import qualified Data.Maybe.Strict           as S
import qualified Data.ByteString             as BS
import qualified Data.HashMap.Strict         as HM
import           Control.Lens                hiding (Strict)
import           Data.Tuple.Strict
import qualified System.Log.Logger           as LOG

import           Puppet.Interpreter.Types
import           Puppet.PP
import           Puppet.Paths
import           Puppet.Parser.Types

initialState :: Facts
             -> Container T.Text -- ^ Server settings
             -> InterpreterState
initialState facts settings = InterpreterState baseVars initialclass mempty [ContRoot] dummypos mempty [] []
    where
        callervars = HM.fromList [("caller_module_name", PString "::" :!: dummypos :!: ContRoot), ("module_name", PString "::" :!: dummypos :!: ContRoot)]
        factvars = fmap (\x -> x :!: initialPPos "facts" :!: ContRoot) facts
        settingvars = fmap (\x -> PString x :!: initialPPos "settings" :!: ContClass "settings") settings
        baseVars = HM.fromList [ ("::", ScopeInformation (factvars `mappend` callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
                               , ("settings", ScopeInformation settingvars mempty mempty (CurContainer (ContClass "settings") mempty) mempty S.Nothing)
                               ]
        initialclass = mempty & at "::" ?~ (IncludeStandard :!: dummypos)


getCurContainer :: InterpreterMonad CurContainer
{-# INLINABLE getCurContainer #-}
getCurContainer = do
    scp <- getScopeName
    preuse (scopes . ix scp . scopeContainer) >>= \case
        Just x -> return x
        Nothing -> throwPosError ("Internal error: can't find the current container for" <+> green (string (T.unpack scp)))

getPuppetPathes :: InterpreterMonad PuppetDirPaths
getPuppetPathes = singleton PuppetPaths

warn :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
warn d = tell [LOG.WARNING :!: d]

debug :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
debug d = tell [LOG.DEBUG :!: d]

logWriter :: (Monad m, MonadWriter InterpreterWriter m) => LOG.Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

safeDecodeUtf8 :: BS.ByteString -> InterpreterMonad T.Text
{-# INLINABLE safeDecodeUtf8 #-}
safeDecodeUtf8 i = return (T.decodeUtf8 i)

-- | Throws an error if we are in strict mode
-- A warning in permissive mode
checkStrict :: Doc -- ^ The warning message.
            -> Doc -- ^ The error message.
            -> InterpreterMonad ()
checkStrict wrn err = do
    extMod <- isExternalModule
    let priority = if extMod then LOG.NOTICE else LOG.WARNING
    str <- singleton IsStrict
    if str && not extMod
        then throwPosError err
        else do
          srcname <- use (curPos._1.lSourceName)
          logWriter priority (wrn <+> "at" <+> string srcname)

isExternalModule :: InterpreterMonad Bool
isExternalModule =
    getScope >>= \case
      ContClass n      -> isExternal n
      ContDefine n _ _ -> isExternal n
      _                -> return False
    where
      isExternal = singleton . IsExternalModule . head . T.splitOn "::"

getScopeName :: InterpreterMonad T.Text
getScopeName = fmap scopeName getScope

getScope :: InterpreterMonad CurContainerDesc
{-# INLINABLE getScope #-}
getScope = use curScope >>= \s -> if null s
                                      then throwPosError "Internal error: empty scope!"
                                      else return (head s)

scopeName :: CurContainerDesc -> T.Text
scopeName (ContRoot        ) = "::"
scopeName (ContImported x  ) = "::imported::" `T.append` scopeName x
scopeName (ContClass x     ) = x
scopeName (ContDefine dt dn _) = "#define/" `T.append` dt `T.append` "/" `T.append` dn
scopeName (ContImport _ x  ) = "::import::" `T.append` scopeName x

dummypos :: PPosition
dummypos = initialPPos "dummy"

fnull :: (Eq x, Monoid x) => x -> Bool
{-# INLINABLE fnull #-}
fnull = (== mempty)

rcurcontainer :: Resource -> CurContainerDesc
rcurcontainer r = fromMaybe ContRoot (r ^? rscope . _head)

eitherDocIO :: IO (S.Either PrettyError a) -> IO (S.Either PrettyError a)
eitherDocIO computation = (computation >>= check) `catch` (\e -> return $ S.Left $ PrettyError $ dullred $ text $ show (e :: SomeException))
    where
        check (S.Left r) = return (S.Left r)
        check (S.Right x) = return (S.Right x)
