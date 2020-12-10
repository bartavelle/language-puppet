{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Puppet.Interpreter.Types (
  -- * Operational state
   InterpreterState(InterpreterState)
 , scopes
 , definedResources
 , nestedDeclarations
 , resModifiers
 , extraRelations
 , curScope
 , curPos
 , loadedClasses
 -- * Operational reader
 , InterpreterReader(InterpreterReader)
 , readerNativeTypes
 , readerGetStatement
 , readerGetTemplate
 , readerPdbApi
 , readerExternalFunc
 , readerNodename
 , readerHieraQuery
 , readerIoMethods
 , readerIgnoredModules
 , readerExternalModules
 , readerIsStrict
 , readerPuppetPaths
 , readerFacts
 , readerRebaseFile
 -- * Interpreter monad
 , InterpreterMonad
 , InterpreterWriter
 , InterpreterInstr(..)
 , Strictness(..)
 -- * Io methods
 , IoMethods(IoMethods)
 , ioGetCurrentCallStack
 , ioReadFile
 , ioTraceEvent
 , MonadThrowPos(..)
 -- * Resource modifier
 , ResourceModifier(ResourceModifier)
 , rmResType
 , rmDeclaration
 , rmSearch
 , rmType
 , rmMutation
 , rmModifierType
 , ModifierType(..)
 , OverrideType(..)
 , ResourceCollectorType(..)
 , ClassIncludeType(..)
 , RSearchExpression(..)
 -- * Scope information
 , ScopeInformation(ScopeInformation)
 , scopeResDefaults
 , scopeVariables
 , scopeParent
 , scopeOverrides
 , scopeContainer
 , scopeExtraTags
 , CurContainer(CurContainer)
 , cctype
 , cctags
 -- * Resource default
 , ResDefaults(ResDefaults)
 , resDefValues
 , resDefSrcScope
 , resDefPos
 , resDefType
 , ResRefOverride(..)
 , ScopeEnteringContext(..)
 , TopLevelType(..)
 -- * Hiera
 , HieraQueryLayers(..)
 , globalLayer
 , environmentLayer
 , moduleLayer
 -- * Template
 , TemplateSource(..)
 -- * Re-export
 , module Puppet.Language
) where

import           XPrelude.Extra
import           XPrelude.PP

import qualified Control.Monad.Fail as Fail
import           Control.Monad.Operational
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Class
import           Data.Aeson                 as A
import qualified Data.Either.Strict         as S
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as List
import qualified Data.Maybe.Strict          as S
import qualified GHC.Show
import qualified GHC.Stack
import qualified System.Log.Logger          as Log

import           Facter
import           Hiera.Server
import           Puppet.Language
import           Puppet.Parser.Types
import           PuppetDB


-- | The intepreter can run in two modes : a strict mode (recommended), and
-- a permissive mode.
data Strictness
  = Strict
  | Permissive
  deriving (Show, Eq)

instance FromJSON Strictness where
  parseJSON (Bool True)  = pure Strict
  parseJSON (Bool False) = pure Permissive
  parseJSON _            = mzero

data RSearchExpression
  = REqualitySearch !Text !PValue
  | RNonEqualitySearch !Text !PValue
  | RAndSearch !RSearchExpression !RSearchExpression
  | ROrSearch !RSearchExpression !RSearchExpression
  | RAlwaysTrue
  deriving (Show, Eq)

-- | Puppet has two main ways to declare classes: include-like and resource-like.
--
-- See <https://docs.puppetlabs.com/puppet/latest/reference/lang_classes.html#include-like-vs-resource-like puppet reference>.
data ClassIncludeType
  = ClassIncludeLike -- ^ Using the include or contain function
  | ClassResourceLike -- ^ Resource like declaration
  deriving (Eq)

-- | Differentiate the distinct top level types such as node, define or class.
data TopLevelType
  = TopNode -- ^ For node entries
  | TopDefine -- ^ For defines
  | TopClass -- ^ For classes
  deriving (Generic, Eq)

instance Hashable TopLevelType

-- | From the evaluation of Resource Default Declaration.
data ResDefaults = ResDefaults
  { _resDefType     :: !Text
  , _resDefSrcScope :: !Text
  , _resDefValues   :: !(Container PValue)
  , _resDefPos      :: !PPosition
  }

-- | From the evaluation of Resource Override Declaration.
data ResRefOverride = ResRefOverride
  { _rrid     :: !RIdentifier
  , _rrparams :: !(Container PValue)
  , _rrpos    :: !PPosition
  } deriving (Eq)

data ScopeEnteringContext
  = SENormal
  | SEChild !Text -- ^ We enter the scope as the child of another class
  | SEParent !Text -- ^ We enter the scope as the parent of another class

-- | The type of the container together with its tags.
data CurContainer = CurContainer
  { _cctype :: !CurContainerDesc
  , _cctags :: !(HashSet Text)
  } deriving (Eq)

data ScopeInformation = ScopeInformation
  { _scopeVariables   :: !(Container (Pair (Pair PValue PPosition) CurContainerDesc))
  , _scopeResDefaults :: !(Container ResDefaults)
  , _scopeExtraTags   :: !(HashSet Text)
  , _scopeContainer   :: !CurContainer
  , _scopeOverrides   :: !(HashMap RIdentifier ResRefOverride)
  , _scopeParent      :: !(S.Maybe Text)
  }

data InterpreterState = InterpreterState
  { _scopes             :: !(Container ScopeInformation)
  , _loadedClasses      :: !(Container (Pair ClassIncludeType PPosition))
  , _definedResources   :: !(HM.HashMap RIdentifier Resource)
  , _curScope           :: ![CurContainerDesc]
  , _curPos             :: !PPosition
  , _nestedDeclarations :: !(HashMap (TopLevelType, Text) Statement)
  , _extraRelations     :: ![LinkInformation]
  , _resModifiers       :: ![ResourceModifier]
  }

data IoMethods m = IoMethods
  { _ioGetCurrentCallStack :: m [String]
  , _ioReadFile            :: [Text] -> m (Either String Text)
  , _ioTraceEvent          :: String -> m ()
  }

-- | All available queries including the global and module layer
-- The environment layer is not implemented.
--
-- The datatype belongs to the "Puppet.Interpreter" module because it serves to implement how Hiera is used within Puppet.
data HieraQueryLayers m = HieraQueryLayers
  { _globalLayer :: HieraQueryFunc m
  , _environmentLayer :: HieraQueryFunc m
  , _moduleLayer :: Container (HieraQueryFunc m)
  }

-- | Whether the template source is specified 'inline' or loaded from a file.
data  TemplateSource= Inline Text | Filename FilePath

data InterpreterReader m = InterpreterReader
  { _readerNativeTypes     :: !(Container NativeTypeMethods)
  , _readerGetStatement    :: TopLevelType -> Text -> m (S.Either PrettyError Statement) -- ^ Access to parsed statements
  , _readerGetTemplate     :: TemplateSource -> InterpreterState -> InterpreterReader m -> m (S.Either PrettyError Text)
  , _readerPdbApi          :: PuppetDBAPI m
  , _readerExternalFunc    :: Container ([PValue] -> InterpreterMonad PValue) -- ^ External func such as stdlib or puppetlabs
  , _readerNodename        :: Text
  , _readerHieraQuery      :: HieraQueryLayers m
  , _readerIoMethods       :: IoMethods m
  , _readerIgnoredModules  :: HashSet Text
  , _readerExternalModules :: HashSet Text
  , _readerIsStrict        :: Bool
  , _readerPuppetPaths     :: PuppetDirPaths
  , _readerRebaseFile      :: Maybe FilePath
  , _readerFacts           :: Container PValue -- ^ Access to the list of facts that were given to the 'Preferences' module
  }

data InterpreterInstr a where
  -- Utility for using what's in 'InterpreterReader'
  GetNativeTypes        :: InterpreterInstr (Container NativeTypeMethods)
  GetStatement          :: TopLevelType -> Text -> InterpreterInstr Statement
  ComputeTemplate       :: TemplateSource-> InterpreterState -> InterpreterInstr Text
  ExternalFunction      :: Text -> [PValue] -> InterpreterInstr PValue
  Facts                 :: InterpreterInstr (Container PValue)
  GetNodeName           :: InterpreterInstr Text
  HieraQuery            :: Container PValue -> Text -> HieraQueryType -> InterpreterInstr (Maybe PValue)
  GetCurrentCallStack   :: InterpreterInstr [String]
  IsIgnoredModule       :: Text -> InterpreterInstr Bool
  IsExternalModule      :: Text -> InterpreterInstr Bool
  IsStrict              :: InterpreterInstr Bool
  PuppetPaths           :: InterpreterInstr PuppetDirPaths
  RebaseFile            :: InterpreterInstr (Maybe FilePath)
  -- error
  ErrorThrow            :: PrettyError -> InterpreterInstr a
  ErrorCatch            :: InterpreterMonad a -> (PrettyError -> InterpreterMonad a) -> InterpreterInstr a
  -- writer
  WriterTell            :: InterpreterWriter -> InterpreterInstr ()
  WriterPass            :: InterpreterMonad (a, InterpreterWriter -> InterpreterWriter) -> InterpreterInstr a
  WriterListen          :: InterpreterMonad a -> InterpreterInstr (a, InterpreterWriter)
  -- puppetdb wrappers  , see 'PuppetDBAPI' for details
  PDBInformation        :: InterpreterInstr Doc
  PDBReplaceCatalog     :: WireCatalog -> InterpreterInstr ()
  PDBReplaceFacts       :: [(NodeName, Facts)] -> InterpreterInstr ()
  PDBDeactivateNode     :: NodeName -> InterpreterInstr ()
  PDBGetFacts           :: Query FactField -> InterpreterInstr [FactInfo]
  PDBGetResources       :: Query ResourceField -> InterpreterInstr [Resource]
  PDBGetNodes           :: Query NodeField -> InterpreterInstr [NodeInfo]
  PDBCommitDB           :: InterpreterInstr ()
  PDBGetResourcesOfNode :: NodeName -> Query ResourceField -> InterpreterInstr [Resource]
  -- Reading the first file that can be read in a list
  ReadFile              :: [Text] -> InterpreterInstr Text
  -- Tracing events
  TraceEvent            :: String -> InterpreterInstr ()

-- | The main monad
type InterpreterMonad = ProgramT InterpreterInstr (State InterpreterState)

instance Fail.MonadFail InterpreterMonad where
    fail = throwError . PrettyError . ppstring

instance MonadError PrettyError InterpreterMonad where
  throwError = singleton . ErrorThrow
  catchError a c = singleton (ErrorCatch a c)

-- | Log
type InterpreterWriter = [Pair Log.Priority Doc]
instance MonadWriter InterpreterWriter InterpreterMonad where
  tell = singleton . WriterTell
  pass = singleton . WriterPass
  listen = singleton . WriterListen

data ResourceModifier = ResourceModifier
  { _rmResType      :: !Text
  , _rmModifierType :: !ModifierType
  , _rmType         :: !ResourceCollectorType
  , _rmSearch       :: !RSearchExpression
  , _rmMutation     :: !(Resource -> InterpreterMonad Resource)
  , _rmDeclaration  :: !PPosition
  }

instance Show ResourceModifier where
  show (ResourceModifier rt mt ct se _ p) = List.unwords ["ResourceModifier", show rt, show mt, show ct, "(" ++ show se ++ ")", "???", show p]

data ModifierType
  = ModifierCollector -- ^ For collectors, optional resources
  | ModifierMustMatch -- ^ For stuff like realize
  deriving (Show, Eq)

data OverrideType
  = CantOverride -- ^ Overriding forbidden, will throw an error
  | Replace -- ^ Can silently replace
  | CantReplace -- ^ Silently ignore errors
  | AppendAttribute -- ^ Can append values
  deriving (Show, Eq)

data ResourceCollectorType
  = RealizeVirtual
  | RealizeCollected
  | DontRealize
  deriving (Show, Eq)

makeLenses ''ResDefaults
makeLenses ''HieraQueryLayers
makeLenses ''ResourceModifier
makeLenses ''InterpreterReader
makeLenses ''IoMethods
makeLenses ''CurContainer
makeLenses ''ScopeInformation
makeLenses ''InterpreterState


class Monad m => MonadThrowPos m where
  throwPosError :: Doc -> m a

-- Useful for mocking for instance in a REPL
instance MonadThrowPos (Either Doc) where
  throwPosError = Left

class MonadStack m where
  getCurrentCallStack :: m [String]

instance MonadStack InterpreterMonad where
  getCurrentCallStack = singleton GetCurrentCallStack

instance MonadThrowPos InterpreterMonad where
  throwPosError s = do
    p <- use (curPos . _1)
    stack <- getCurrentCallStack
    let dstack = if null stack
                   then line
                   else mempty </> ppstring (GHC.Stack.renderStack stack)
    throwError (PrettyError (s <+> "at" <+> showPos p <> dstack))
