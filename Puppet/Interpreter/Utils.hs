{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}

-- | The module should not depend on the Interpreter module. It is an
-- internal module and should not be used if expecting a stable API.
module Puppet.Interpreter.Utils where

import           Puppet.Prelude

import           Control.Monad.Operational
import           Control.Monad.Writer.Class
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as List
import qualified Data.Maybe.Strict          as S
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified System.Log.Logger          as Log

import           Puppet.Interpreter.Types
import           Puppet.Parser.Types
import           Puppet.Parser.Utils
import           Puppet.Paths
import           Puppet.PP

initialState :: Facts
             -> Container Text -- ^ Server settings
             -> InterpreterState
initialState facts settings = InterpreterState baseVars initialclass mempty [ContRoot] dummyppos mempty [] []
    where
        callervars = HM.fromList [("caller_module_name", PString "::" :!: dummyppos :!: ContRoot), ("module_name", PString "::" :!: dummyppos :!: ContRoot)]
        factvars =
          -- add the `facts` key: https://docs.puppet.com/puppet/4.10/lang_facts_and_builtin_vars.html#accessing-facts-from-puppet-code
          let facts' = HM.insert "facts" (PHash facts) facts
          in fmap (\x -> x :!: initialPPos "facts" :!: ContRoot) facts'
        settingvars = fmap (\x -> PString x :!: initialPPos "settings" :!: ContClass "settings") settings
        baseVars = HM.fromList [ ("::", ScopeInformation (factvars `mappend` callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
                               , ("settings", ScopeInformation settingvars mempty mempty (CurContainer (ContClass "settings") mempty) mempty S.Nothing)
                               ]
        initialclass = mempty & at "::" ?~ (ClassIncludeLike :!: dummyppos)


getModulename :: RIdentifier -> Text
getModulename (RIdentifier t n) =
    let gm x = case Text.splitOn "::" x of
                   []    -> x
                   (y:_) -> y
    in case t of
        "class" -> gm n
        _       -> gm t


extractPrism :: Doc -> Prism' a b -> a -> InterpreterMonad b
extractPrism msg p a = case preview p a of
    Just b  -> return b
    Nothing -> throwPosError ("Could not extract prism in" <+> msg)

-- Scope
popScope :: InterpreterMonad ()
popScope = curScope %= List.tail

pushScope :: CurContainerDesc -> InterpreterMonad ()
pushScope s = curScope %= (s :)

getScopeName :: InterpreterMonad Text
getScopeName = scopeName <$> getScope

scopeName :: CurContainerDesc -> Text
scopeName (ContRoot        ) = "::"
scopeName (ContImported x  ) = "::imported::" `Text.append` scopeName x
scopeName (ContClass x     ) = x
scopeName (ContDefine dt dn _) = "#define/" `Text.append` dt `Text.append` "/" `Text.append` dn
scopeName (ContImport _ x  ) = "::import::" `Text.append` scopeName x

moduleName :: CurContainerDesc -> Text
moduleName (ContRoot        )  = "::"
moduleName (ContImported x  )  = moduleName x
moduleName (ContClass x     )  = x
moduleName (ContDefine dt _ _) = dt
moduleName (ContImport _ x  )  = moduleName x

getScope :: InterpreterMonad CurContainerDesc
{-# INLINABLE getScope #-}
getScope =
  use curScope >>= \s -> if null s
                         then throwPosError "Internal error: empty scope!"
                         else pure (List.head s)

getCurContainer :: InterpreterMonad CurContainer
{-# INLINABLE getCurContainer #-}
getCurContainer = do
    scp <- getScopeName
    preuse (scopes . ix scp . scopeContainer) >>= \case
        Just x -> return x
        Nothing -> throwPosError ("Internal error: can't find the current container for" <+> green (string (Text.unpack scp)))

rcurcontainer :: Resource -> CurContainerDesc
rcurcontainer r = fromMaybe ContRoot (r ^? rscope . _head)

-- Singleton getters available in the InterpreterMonad --
getPuppetPaths :: InterpreterMonad PuppetDirPaths
getPuppetPaths = singleton PuppetPaths

getNodeName:: InterpreterMonad NodeName
getNodeName = singleton GetNodeName

isIgnoredModule :: Text -> InterpreterMonad Bool
isIgnoredModule m = singleton (IsIgnoredModule m)

-- | Throws an error if we are in strict mode
-- A warning in permissive mode
checkStrict :: Doc -- ^ The warning message.
            -> Doc -- ^ The error message.
            -> InterpreterMonad ()
checkStrict wrn err = do
    extMod <- isExternalModule
    let priority = if extMod then Log.NOTICE else Log.WARNING
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
      isExternal = singleton . IsExternalModule . List.head . Text.splitOn "::"


-- Logging --
warn :: MonadWriter InterpreterWriter m => Doc -> m ()
warn d = tell [Log.WARNING :!: d]

debug :: MonadWriter InterpreterWriter m => Doc -> m ()
debug d = tell [Log.DEBUG :!: d]

logWriter :: MonadWriter InterpreterWriter m => Log.Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

-- General --
isEmpty :: (Eq x, Monoid x) => x -> Bool
isEmpty = (== mempty)

safeDecodeUtf8 :: ByteString -> InterpreterMonad Text
{-# INLINABLE safeDecodeUtf8 #-}
safeDecodeUtf8 i = return (Text.decodeUtf8 i)

dropInitialColons :: Text -> Text
dropInitialColons t = fromMaybe t (Text.stripPrefix "::" t)

normalizeRIdentifier :: Text -> Text -> RIdentifier
normalizeRIdentifier = RIdentifier . dropInitialColons
