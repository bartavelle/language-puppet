{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}

-- | Internal helpers module.
module Puppet.Interpreter.Helpers where

import           XPrelude

import           Control.Monad.Operational
import           Control.Monad.Writer.Class
import qualified Data.HashMap.Strict        as Map
import qualified Data.Vector                as Vector
import qualified Data.List                  as List
import qualified Data.Maybe.Strict          as S
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified System.Log.Logger          as Log

import           Facter
import           Puppet.Interpreter.Types
import           Puppet.Parser


initialState :: Facts
             -> Container Text -- ^ Server settings
             -> InterpreterState
initialState facts settings =
  InterpreterState baseVars initialclass mempty [ContRoot] dummyppos mempty [] []
  where
    callervars = Map.fromList [("caller_module_name", PString "::" :!: dummyppos :!: ContRoot), ("module_name", PString "::" :!: dummyppos :!: ContRoot)]
    factvars =
      -- add the `facts` key: https://docs.puppet.com/puppet/4.10/lang_facts_and_builtin_vars.html#accessing-facts-from-puppet-code
      let facts' = Map.insert "facts" (PHash facts) facts
      in fmap (\x -> x :!: initialPPos "facts" :!: ContRoot) facts'
    settingvars = fmap (\x -> PString x :!: initialPPos "settings" :!: ContClass "settings") settings
    baseVars = Map.fromList [ ("::", ScopeInformation (factvars `mappend` callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
                           , ("settings", ScopeInformation settingvars mempty mempty (CurContainer (ContClass "settings") mempty) mempty S.Nothing)
                           ]
    initialclass = mempty & at "::" ?~ (ClassIncludeLike :!: dummyppos)

getModulename :: RIdentifier -> Text
getModulename (RIdentifier t n) =
  let gm x =
        case Text.splitOn "::" x of
          []    -> x
          (y:_) -> y
  in case t of
    "class" -> gm n
    _       -> gm t

extractPrism :: Doc -> Prism' a b -> a -> InterpreterMonad b
extractPrism msg p a =
  case preview p a of
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

containerModName :: CurContainerDesc -> Text
containerModName (ContRoot        )  = "::"
containerModName (ContImported x  )  = containerModName x
containerModName (ContClass x     )  = x
containerModName (ContDefine dt _ _) = dt
containerModName (ContImport _ x  )  = containerModName x

getScope :: InterpreterMonad CurContainerDesc
{-# INLINABLE getScope #-}
getScope =
  use curScope >>= \s ->
    if null s
      then throwPosError "Internal error: empty scope!"
      else pure (List.head s)

getCurContainer :: InterpreterMonad CurContainer
{-# INLINABLE getCurContainer #-}
getCurContainer = do
  scp <- getScopeName
  preuse (scopes . ix scp . scopeContainer) >>= \case
    Just x -> return x
    Nothing -> throwPosError ("Internal error: can't find the current container for" <+> green (ppline scp))

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
  let priority =
        if extMod
          then Log.NOTICE
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
warn :: MonadWriter InterpreterWriter m => Doc -> m ()
warn d = tell [Log.WARNING :!: d]

debug :: MonadWriter InterpreterWriter m => Doc -> m ()
debug d = tell [Log.DEBUG :!: d]

logWriter :: MonadWriter InterpreterWriter m => Log.Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

safeDecodeUtf8 :: ByteString -> InterpreterMonad Text
{-# INLINABLE safeDecodeUtf8 #-}
safeDecodeUtf8 i = return (Text.decodeUtf8 i)

normalizeRIdentifier :: Text -> Text -> RIdentifier
normalizeRIdentifier = RIdentifier . dropInitialColons


extractFromState :: InterpreterState -> Maybe (Text, Container ScopeInformation)
extractFromState s =
  let cscope = s ^. curScope
  in if null cscope
       then Nothing
       else let scope_name = scopeName (List.head cscope)
                classes = (PArray . Vector.fromList . map PString . Map.keys) (s ^. loadedClasses)
                scps = s ^. scopes
                container_desc = fromMaybe ContRoot (scps ^? ix scope_name . scopeContainer . cctype) -- get the current containder description
                cscps = scps & ix scope_name . scopeVariables . at "classes" ?~ ( classes :!: dummyppos :!: container_desc )
            in  Just (scope_name, cscps)
