{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

-- | Internal helpers module.
module Puppet.Interpreter.Helpers where

import Control.Monad.Operational
import Control.Monad.Writer.Class
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Maybe.Strict as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Facter
import GHC.Err as Err
import Puppet.Interpreter.Types
import qualified System.Log.Logger as Log
import XPrelude hiding (key)

initialState ::
  Facts ->
  -- | Server settings
  Container Text ->
  InterpreterState
initialState facts settings =
  InterpreterState baseVars initialclass mempty [ContRoot] (initialPPos mempty) mempty [] []
  where
    callervars = Map.fromList [("caller_module_name", (PString "::" :!: initialPPos mempty) :!: ContRoot), ("module_name", (PString "::" :!: initialPPos mempty) :!: ContRoot)]
    factvars =
      -- add the `facts` key: https://docs.puppet.com/puppet/4.10/lang_facts_and_builtin_vars.html#accessing-facts-from-puppet-code
      let facts' = Map.insert "facts" (PHash facts) facts
       in fmap (\x -> (x :!: initialPPos "facts") :!: ContRoot) facts'
    settingvars = fmap (\x -> (PString x :!: initialPPos "settings") :!: ContClass "settings") settings
    baseVars =
      Map.fromList
        [ ("::", ScopeInformation (factvars `mappend` callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing),
          ("settings", ScopeInformation settingvars mempty mempty (CurContainer (ContClass "settings") mempty) mempty S.Nothing)
        ]
    initialclass = mempty & at "::" ?~ (ClassIncludeLike :!: initialPPos mempty)

getModulename :: RIdentifier -> Text
getModulename (RIdentifier t n) =
  let gm x =
        case Text.splitOn "::" x of
          [] -> x
          (y : _) -> y
   in case t of
        "class" -> gm n
        _ -> gm t

extractPrism :: Doc -> Prism' a b -> a -> InterpreterMonad b
extractPrism msg p a =
  case preview p a of
    Just b -> return b
    Nothing -> throwPosError ("Could not extract prism in" <+> msg)

-- Scope
popScope :: InterpreterMonad ()
popScope = curScope %= List.tail

pushScope :: CurContainerDesc -> InterpreterMonad ()
pushScope s = curScope %= (s :)

getScopeName :: InterpreterMonad Text
getScopeName = scopeName <$> getScope

scopeName :: CurContainerDesc -> Text
scopeName ContRoot = "::"
scopeName (ContImported x) = "::imported::" `Text.append` scopeName x
scopeName (ContClass x) = x
scopeName (ContDefine dt dn _) = "#define/" `Text.append` dt `Text.append` "/" `Text.append` dn
scopeName (ContImport _ x) = "::import::" `Text.append` scopeName x

containerModName :: CurContainerDesc -> Text
containerModName ContRoot = "::"
containerModName (ContImported x) = containerModName x
containerModName (ContClass x) = x
containerModName (ContDefine dt _ _) = dt
containerModName (ContImport _ x) = containerModName x

getScope :: InterpreterMonad CurContainerDesc
{-# INLINEABLE getScope #-}
getScope =
  use curScope >>= \s ->
    if null s
      then throwPosError "Internal error: empty scope!"
      else pure (List.head s)

getCurContainer :: InterpreterMonad CurContainer
{-# INLINEABLE getCurContainer #-}
getCurContainer = do
  scp <- getScopeName
  preuse (scopes . ix scp . scopeContainer) >>= \case
    Just x -> return x
    Nothing -> throwPosError ("Internal error: can't find the current container for" <+> ppline scp)

rcurcontainer :: Resource -> CurContainerDesc
rcurcontainer r = fromMaybe ContRoot (r ^? rscope . _head)

-- Singleton getters available in the InterpreterMonad --
getPuppetPaths :: InterpreterMonad PuppetDirPaths
getPuppetPaths = singleton PuppetPaths

getNodeName :: InterpreterMonad NodeName
getNodeName = singleton GetNodeName

-- | Give key such as "os.family"
-- look an hash of facts to retrieve deepest PValue
lookupFacts :: Text -> HashMap Text PValue -> Maybe PValue
lookupFacts key facts =
  let (k0, ks) = case Text.splitOn "." key of
        a : b -> (a, b)
        _ -> Err.error "should not happen"
      f k = \case
        Just (PHash h) -> Map.lookup k h
        x -> x
   in List.foldr f (Map.lookup k0 facts) ks

-- | Ask the value of a fact given a specified key
-- The fact set comes from the reader used by the interpreter monad.
askFact :: Text -> InterpreterMonad (Maybe PValue)
askFact key = do
  facts <- singleton Facts
  pure $ lookupFacts key facts

isIgnoredModule :: Text -> InterpreterMonad Bool
isIgnoredModule m = singleton (IsIgnoredModule m)

-- | Throws an error if we are in strict mode
-- A warning in permissive mode
checkStrict ::
  -- | The warning message.
  Doc ->
  -- | The error message.
  Doc ->
  InterpreterMonad ()
checkStrict wrn err = do
  extMod <- isExternalModule
  let priority =
        if extMod
          then Log.INFO
          else Log.WARNING
  str <- singleton IsStrict
  if str && not extMod
    then throwPosError err
    else do
      srcname <- use (curPos . _1 . _sourceName)
      logWriter priority (wrn <+> "at" <+> ppstring srcname)

isExternalModule :: InterpreterMonad Bool
isExternalModule =
  getScope >>= \case
    ContClass n -> isExternal n
    ContDefine n _ _ -> isExternal n
    _ -> return False
  where
    isExternal = singleton . IsExternalModule . List.head . Text.splitOn "::"

-- Logging --
error :: (MonadWriter InterpreterWriter m) => Doc -> m ()
error d = tell [Log.ERROR :!: d]

critical :: (MonadWriter InterpreterWriter m) => Doc -> m ()
critical d = tell [Log.CRITICAL :!: d]

warn :: (MonadWriter InterpreterWriter m) => Doc -> m ()
warn d = tell [Log.WARNING :!: d]

debug :: (MonadWriter InterpreterWriter m) => Doc -> m ()
debug d = tell [Log.DEBUG :!: d]

logWriter :: (MonadWriter InterpreterWriter m) => Log.Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

safeDecodeUtf8 :: ByteString -> InterpreterMonad Text
{-# INLINEABLE safeDecodeUtf8 #-}
safeDecodeUtf8 i = pure (Text.decodeUtf8 i)

normalizeRIdentifier :: Text -> Text -> RIdentifier
normalizeRIdentifier = RIdentifier . dropInitialColons

extractScope :: InterpreterState -> Maybe (Text, Container ScopeInformation)
extractScope s =
  let cscope = s ^. curScope
   in if null cscope
        then Nothing
        else
          let scope_name = scopeName (List.head cscope)
              classes = (PArray . Vector.fromList . map PString . Map.keys) (s ^. loadedClasses)
              scps = s ^. scopes
              container_desc = fromMaybe ContRoot (scps ^? ix scope_name . scopeContainer . cctype) -- get the current containder description
              cscps = scps & ix scope_name . scopeVariables . at "classes" ?~ ((classes :!: initialPPos mempty) :!: container_desc)
           in Just (scope_name, cscps)
