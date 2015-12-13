{-# LANGUAGE AutoDeriveTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Puppet.Interpreter.Types (
  -- * Record & lenses
   HasResource(..)
 , Resource(Resource)
 , HasResDefaults(..)
 , ResDefaults(ResDefaults)
 , HasLinkInformation(..)
 , LinkInformation(LinkInformation)
 , HasRIdentifier(..)
 , RIdentifier(RIdentifier)
 , HasScopeInformation(..)
 , ScopeInformation(ScopeInformation)
 , ScopeEnteringContext(..)
 , HasResourceModifier(..)
 , ResourceModifier(ResourceModifier)
 , HasIoMethods(..)
 , IoMethods(IoMethods)
 , HasCurContainer(..)
 , CurContainer(CurContainer)
 , HasNativeTypeMethods(..)
 , NativeTypeMethods(NativeTypeMethods)
 , NodeInfo(NodeInfo)
 , HasNodeInfo(..)
 , FactInfo(FactInfo)
 , HasFactInfo(..)
 , HasWireCatalog(..)
  -- ** Operational instructions
 , InterpreterInstr(..)
 , HasInterpreterReader(..)
 , InterpreterReader(InterpreterReader)
 , HasInterpreterState(..)
 , InterpreterState(InterpreterState)
  -- * Sum types
 , PValue(..)
 , CurContainerDesc(..)
 , ResourceCollectorType(..)
 , RSearchExpression(..)
 , Query(..)
 , ModifierType(..)
 , NodeField
 , Strictness(..)
 , HieraQueryType(..)
 , WireCatalog(..)
 , TopLevelType(..)
 , FactField(..)
 , ResRefOverride(..)
 , ResourceField(..)
 , OverrideType(..)
 , ClassIncludeType(..)
  -- ** PuppetDB
 , PuppetEdge(PuppetEdge)
 , PuppetDBAPI(..)
  -- * newtype & synonym
 , PrettyError(..)
 , InterpreterMonad
 , InterpreterWriter
 , FinalCatalog
 , NativeTypeValidate
 , NodeName
 , Container
 , HieraQueryFunc
 , Scope
 , Facts
 , EdgeMap
  -- * Classes
 , MonadThrowPos(..)
  -- * definitions
 , metaparameters
 , showPos
) where

import           Control.Concurrent.MVar     (MVar)
import           Control.Exception
import           Control.Lens                hiding (Strict)
import           Control.Monad.Except
import           Control.Monad.Operational
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Either
import           Control.Monad.Writer.Class
import           Data.Aeson                  as A
import           Data.Aeson.Lens
import qualified Data.Either.Strict          as S
import           Data.Hashable
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Maybe.Strict           as S
import           Data.Monoid
import           Data.Scientific
import           Data.String                 (IsString (..))
import           Data.Text (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Clock
import qualified Data.Traversable            as TR
import           Data.Tuple.Strict
import qualified Data.Vector                 as V
import           Foreign.Ruby.Helpers
import           GHC.Generics                hiding (to)
import           GHC.Stack
import qualified Scripting.Lua               as Lua
import           Servant.Common.Text
import qualified System.Log.Logger           as LOG
import           Text.Megaparsec.Pos

import           Puppet.Parser.PrettyPrinter
import           Puppet.Parser.Types
import           Puppet.Paths
import           Puppet.PP                   hiding (rational)
import           Puppet.Utils

metaparameters :: HS.HashSet Text
metaparameters = HS.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"]

type NodeName = Text
type Container = HM.HashMap Text
type Scope = Text
type Facts = Container PValue

newtype PrettyError = PrettyError { getError :: Doc }

instance Show PrettyError where
    show = show . getError

instance Monoid PrettyError where
    mempty = PrettyError mempty
    mappend a b = PrettyError $ getError a <+> getError b

instance IsString PrettyError where
    fromString = PrettyError . string

instance Exception PrettyError


data PValue = PBoolean !Bool
            | PUndef
            | PString !Text -- integers and doubles are internally serialized as strings by puppet
            | PResourceReference !Text !T.Text
            | PArray !(V.Vector PValue)
            | PHash !(Container PValue)
            | PNumber !Scientific
            deriving (Eq, Show)

instance IsString PValue where
    fromString = PString . T.pack

instance AsNumber PValue where
    _Number = prism num2PValue toNumber
        where
            num2PValue :: Scientific -> PValue
            num2PValue = PNumber
            toNumber :: PValue -> Either PValue Scientific
            toNumber (PNumber n) = Right n
            toNumber p@(PString x) = case text2Scientific x of
                                         Just o -> Right o
                                         _      -> Left p
            toNumber p = Left p

-- | The different kind of hiera queries
data HieraQueryType = Priority   -- ^ standard hiera query
                    | ArrayMerge -- ^ hiera_array
                    | HashMerge  -- ^ hiera_hash

-- | The type of the Hiera API function
type HieraQueryFunc m = Container Text -- ^ All the variables that Hiera can interpolate, the top level ones being prefixed with ::
                     -> Text -- ^ The query
                     -> HieraQueryType
                     -> m (S.Either PrettyError (Maybe PValue))

-- | The intepreter can run in two modes : a strict mode (recommended), and
-- a permissive mode. The permissive mode let known antipatterns work with
-- the interpreter.
data Strictness = Strict | Permissive
                deriving (Show, Eq)

instance FromJSON Strictness where
  parseJSON (Bool True) = pure Strict
  parseJSON (Bool False) = pure Permissive
  parseJSON _ = mzero

data RSearchExpression = REqualitySearch !Text !PValue
                       | RNonEqualitySearch !Text !PValue
                       | RAndSearch !RSearchExpression !RSearchExpression
                       | ROrSearch !RSearchExpression !RSearchExpression
                       | RAlwaysTrue
                       deriving (Show, Eq)

-- | Puppet has two main ways to declare classes: include-like and resource-like
-- https://docs.puppetlabs.com/puppet/latest/reference/lang_classes.html#include-like-vs-resource-like
data ClassIncludeType = ClassIncludeLike  -- ^ using the include or contain function
                      | ClassResourceLike -- ^ resource like declaration
                      deriving (Eq)

-- |This type is used to differenciate the distinct top level types that are
-- exposed by the DSL.
data TopLevelType
    -- |This is for node entries.
    = TopNode
    -- |This is for defines.
    | TopDefine
    -- |This is for classes.
    | TopClass
    deriving (Generic,Eq)

instance Hashable TopLevelType

-- | From the evaluation of Resource Default Declaration
data ResDefaults = ResDefaults
    { _resDefType     :: !Text
    , _resDefSrcScope :: !Text
    , _resDefValues   :: !(Container PValue)
    , _resDefPos      :: !PPosition
    }

-- | From the evaluation of Resource Override Declaration
data ResRefOverride = ResRefOverride
    { _rrid     :: !RIdentifier
    , _rrparams :: !(Container PValue)
    , _rrpos    :: !PPosition
    } deriving (Eq)

data CurContainerDesc = ContRoot -- ^ Contained at node or root level
                      | ContClass !Text -- ^ Contained in a class
                      | ContDefine !Text !T.Text !PPosition -- ^ Contained in a define, along with the position where this define was ... defined
                      | ContImported !CurContainerDesc -- ^ Dummy container for imported resources, so that we know we must update the nodename
                      | ContImport !NodeName !CurContainerDesc -- ^ This one is used when finalizing imported resources, and contains the current node name
                      deriving (Eq, Generic, Ord, Show)

data ScopeEnteringContext = SENormal
                          | SEChild  !Text -- ^ We enter the scope as the child of another class
                          | SEParent !Text -- ^ We enter the scope as the parent of another class

-- | TODO related to Scope: explain ...
data CurContainer = CurContainer
    { _cctype :: !CurContainerDesc
    , _cctags :: !(HS.HashSet Text)
    } deriving (Eq)

data ScopeInformation = ScopeInformation
    { _scopeVariables   :: !(Container (Pair (Pair PValue PPosition) CurContainerDesc))
    , _scopeResDefaults :: !(Container ResDefaults)
    , _scopeExtraTags   :: !(HS.HashSet Text)
    , _scopeContainer   :: !CurContainer
    , _scopeOverrides   :: !(HM.HashMap RIdentifier ResRefOverride)
    , _scopeParent      :: !(S.Maybe Text)
    }

data InterpreterState = InterpreterState
    { _scopes             :: !(Container ScopeInformation)
    , _loadedClasses      :: !(Container (Pair ClassIncludeType PPosition))
    , _definedResources   :: !(HM.HashMap RIdentifier Resource)
    , _curScope           :: ![CurContainerDesc]
    , _curPos             :: !PPosition
    , _nestedDeclarations :: !(HM.HashMap (TopLevelType,Text) Statement)
    , _extraRelations     :: ![LinkInformation]
    , _resMod             :: ![ResourceModifier]
    }

data InterpreterReader m = InterpreterReader
    { _readerNativeTypes     :: !(Container NativeTypeMethods)
    , _readerGetStatement    :: TopLevelType -> Text -> m (S.Either PrettyError Statement)
    , _readerGetTemplate     :: Either Text T.Text -> InterpreterState -> InterpreterReader m -> m (S.Either PrettyError T.Text)
    , _readerPdbApi          :: PuppetDBAPI m
    , _readerExternalFunc    :: Container ([PValue] -> InterpreterMonad PValue)
    , _readerNodename        :: Text
    , _readerHieraQuery      :: HieraQueryFunc m
    , _readerIoMethods       :: IoMethods m
    , _readerIgnoredModules  :: HS.HashSet Text
    , _readerExternalModules :: HS.HashSet Text
    , _readerIsStrict        :: Bool
    , _readerPuppetPaths     :: PuppetDirPaths
    }

data IoMethods m = IoMethods
    { _ioGetCurrentCallStack :: m [String]
    , _ioReadFile            :: [Text] -> m (Either String T.Text)
    , _ioTraceEvent          :: String -> m ()
    , _ioCallLua             :: MVar Lua.LuaState -> Text -> [PValue] -> m (Either String PValue)
    }

data InterpreterInstr a where
    -- Utility for using what's in "InterpreterReader"
    GetNativeTypes      :: InterpreterInstr (Container NativeTypeMethods)
    GetStatement        :: TopLevelType -> Text -> InterpreterInstr Statement
    ComputeTemplate     :: Either Text T.Text -> InterpreterState -> InterpreterInstr T.Text
    ExternalFunction    :: Text -> [PValue] -> InterpreterInstr PValue
    GetNodeName         :: InterpreterInstr Text
    HieraQuery          :: Container Text -> T.Text -> HieraQueryType -> InterpreterInstr (Maybe PValue)
    GetCurrentCallStack :: InterpreterInstr [String]
    IsIgnoredModule     :: Text -> InterpreterInstr Bool
    IsExternalModule    :: Text -> InterpreterInstr Bool
    IsStrict            :: InterpreterInstr Bool
    PuppetPaths         :: InterpreterInstr PuppetDirPaths
    -- error
    ErrorThrow          :: PrettyError -> InterpreterInstr a
    ErrorCatch          :: InterpreterMonad a -> (PrettyError -> InterpreterMonad a) -> InterpreterInstr a
    -- writer
    WriterTell          :: InterpreterWriter -> InterpreterInstr ()
    WriterPass          :: InterpreterMonad (a, InterpreterWriter -> InterpreterWriter) -> InterpreterInstr a
    WriterListen        :: InterpreterMonad a -> InterpreterInstr (a, InterpreterWriter)
    -- puppetdb wrappers, see 'PuppetDBAPI' for details
    PDBInformation      :: InterpreterInstr Doc
    PDBReplaceCatalog   :: WireCatalog -> InterpreterInstr ()
    PDBReplaceFacts     :: [(NodeName, Facts)] -> InterpreterInstr ()
    PDBDeactivateNode   :: NodeName -> InterpreterInstr ()
    PDBGetFacts         :: Query FactField -> InterpreterInstr [FactInfo]
    PDBGetResources     :: Query ResourceField -> InterpreterInstr [Resource]
    PDBGetNodes         :: Query NodeField -> InterpreterInstr [NodeInfo]
    PDBCommitDB         :: InterpreterInstr ()
    PDBGetResourcesOfNode :: NodeName -> Query ResourceField -> InterpreterInstr [Resource]
    -- Reading the first file that can be read in a list
    ReadFile            :: [Text] -> InterpreterInstr T.Text
    -- Tracing events
    TraceEvent          :: String -> InterpreterInstr ()
    -- Calling Lua
    CallLua             :: MVar Lua.LuaState -> Text -> [PValue] -> InterpreterInstr PValue


-- | The main monad
type InterpreterMonad = ProgramT InterpreterInstr (State InterpreterState)
instance MonadError PrettyError InterpreterMonad where
    throwError = singleton . ErrorThrow
    catchError a c = singleton (ErrorCatch a c)

-- | Log
type InterpreterWriter = [Pair LOG.Priority Doc]
instance MonadWriter InterpreterWriter InterpreterMonad where
    tell = singleton . WriterTell
    pass = singleton . WriterPass
    listen = singleton . WriterListen

data RIdentifier = RIdentifier
    { _itype :: !Text
    , _iname :: !Text
    } deriving (Show,Eq,Generic,Ord)

instance Hashable RIdentifier

data ModifierType = ModifierCollector -- ^ For collectors, optional resources
                  | ModifierMustMatch -- ^ For stuff like realize
                  deriving (Show, Eq)

data OverrideType = CantOverride -- ^ Overriding forbidden, will throw an error
                  | Replace -- ^ Can silently replace
                  | CantReplace -- ^ Silently ignore errors

data ResourceCollectorType = RealizeVirtual
                           | RealizeCollected
                           | DontRealize
                           deriving (Show, Eq)


data ResourceModifier = ResourceModifier
    { _rmResType      :: !Text
    , _rmModifierType :: !ModifierType
    , _rmType         :: !ResourceCollectorType
    , _rmSearch       :: !RSearchExpression
    , _rmMutation     :: !(Resource -> InterpreterMonad Resource)
    , _rmDeclaration  :: !PPosition
    }

instance Show ResourceModifier where
  show (ResourceModifier rt mt ct se _ p) = unwords$ [show rt, show mt, show ct, "(" ++ show se ++ ")", show p]

data LinkInformation = LinkInformation
    { _linksrc  :: !RIdentifier
    , _linkdst  :: !RIdentifier
    , _linkType :: !LinkType
    , _linkPos  :: !PPosition
    } deriving Show

type EdgeMap = HM.HashMap RIdentifier [LinkInformation]

{-| A fully resolved puppet resource that will be used in the 'FinalCatalog'. -}
data Resource = Resource
    { _rid         :: !RIdentifier                                    -- ^ Resource name.
    , _ralias      :: !(HS.HashSet Text)                            -- ^ All the resource aliases
    , _rattributes :: !(Container PValue)                             -- ^ Resource parameters.
    , _rrelations  :: !(HM.HashMap RIdentifier (HS.HashSet LinkType)) -- ^ Resource relations.
    , _rscope      :: ![CurContainerDesc]                             -- ^ Resource scope when it was defined, the real container will be the first item
    , _rvirtuality :: !Virtuality
    , _rtags       :: !(HS.HashSet Text)
    , _rpos        :: !PPosition -- ^ Source code position of the resource definition.
    , _rnode       :: !NodeName -- ^ The node were this resource was created, if remote
    }
    deriving (Eq, Show)

type NativeTypeValidate = Resource -> Either PrettyError Resource

-- | Attributes (and providers) of a puppet resource type bundled with validation rules
data NativeTypeMethods = NativeTypeMethods
    { _puppetValidate :: NativeTypeValidate
    , _puppetFields   :: HS.HashSet Text
    }

type FinalCatalog = HM.HashMap RIdentifier Resource

-- | Used to represent a relationship between two resources within the wired format (json).
-- See <http://docs.puppetlabs.com/puppetdb/2.3/api/wire_format/catalog_format_v5.html#data-type-edge>
data PuppetEdge = PuppetEdge RIdentifier RIdentifier LinkType deriving Show

-- | Wire format, see <http://docs.puppetlabs.com/puppetdb/1.5/api/wire_format/catalog_format.html>.
data WireCatalog = WireCatalog
    { _wireCatalogNodename        :: !NodeName
    , _wireCatalogVersion         :: !Text
    , _wireCatalogEdges           :: !(V.Vector PuppetEdge)
    , _wireCatalogResources       :: !(V.Vector Resource)
    , _wireCatalogTransactionUUID :: !Text
    } deriving Show

data FactInfo = FactInfo
    { _factInfoNodename :: !NodeName
    , _factInfoName     :: !Text
    , _factInfoVal      :: !PValue
    }

data NodeInfo = NodeInfo
    { _nodeInfoName        :: !NodeName
    , _nodeInfoDeactivated :: !Bool
    , _nodeInfoCatalogT    :: !(S.Maybe UTCTime)
    , _nodeInfoFactsT      :: !(S.Maybe UTCTime)
    , _nodeInfoReportT     :: !(S.Maybe UTCTime)
    }

data PuppetDBAPI m = PuppetDBAPI
    { pdbInformation     :: m Doc
    , replaceCatalog     :: WireCatalog         -> EitherT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-catalog-version-3>
    , replaceFacts       :: [(NodeName, Facts)] -> EitherT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-facts-version-1>
    , deactivateNode     :: NodeName            -> EitherT PrettyError m () -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#deactivate-node-version-1>
    , getFacts           :: Query FactField     -> EitherT PrettyError m [FactInfo] -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/facts.html#get-v3facts>
    , getResources       :: Query ResourceField -> EitherT PrettyError m [Resource] -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/resources.html#get-v3resources>
    , getNodes           :: Query NodeField     -> EitherT PrettyError m [NodeInfo]
    , commitDB           ::                        EitherT PrettyError m () -- ^ This is only here to tell the test PuppetDB to save its content to disk.
    , getResourcesOfNode :: NodeName -> Query ResourceField -> EitherT PrettyError m [Resource]
    }

-- | Pretty straightforward way to define the various PuppetDB queries
data Query a = QEqual a Text
             | QG a Integer
             | QL a Integer
             | QGE a Integer
             | QLE a Integer
             | QMatch Text T.Text
             | QAnd [Query a]
             | QOr [Query a]
             | QNot (Query a)
             | QEmpty

-- | Fields for the fact endpoint
data FactField = FName
               | FValue
               | FCertname

-- | Fields for the node endpoint
data NodeField = NName | NFact Text

-- | Fields for the resource endpoint
data ResourceField = RTag
                   | RCertname
                   | RParameter Text
                   | RType
                   | RTitle
                   | RExported
                   | RFile
                   | RLine

makeClassy ''RIdentifier
makeClassy ''ResRefOverride
makeClassy ''LinkInformation
makeClassy ''ResDefaults
makeClassy ''ResourceModifier
makeClassy ''NativeTypeMethods
makeClassy ''ScopeInformation
makeClassy ''Resource
makeClassy ''InterpreterState
makeClassy ''InterpreterReader
makeClassy ''IoMethods
makeClassy ''CurContainer
makeClassy ''NodeInfo
makeClassy ''WireCatalog
makeClassy ''FactInfo


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
                         then mempty
                         else mempty </> string (renderStack stack)
        throwError (PrettyError (s <+> "at" <+> showPos p <> dstack))

instance ToRuby PValue where
    toRuby = toRuby . toJSON
instance FromRuby PValue where
    fromRuby = fmap chk . fromRuby
        where
            chk (Left x) = Left x
            chk (Right x) = case fromJSON x of
                                Error rr -> Left rr

                                Success suc -> Right suc

-- JSON INSTANCES --

instance FromJSON PValue where
    parseJSON Null       = return PUndef
    parseJSON (Number n) = return $ PNumber n
    parseJSON (String s) = return (PString s)
    parseJSON (Bool b)   = return (PBoolean b)
    parseJSON (Array v)  = fmap PArray (V.mapM parseJSON v)
    parseJSON (Object o) = fmap PHash (TR.mapM parseJSON o)

instance ToJSON PValue where
    toJSON (PBoolean b)             = Bool b
    toJSON PUndef                   = Null
    toJSON (PString s)              = String s
    toJSON (PResourceReference _ _) = Null -- TODO
    toJSON (PArray r)               = Array (V.map toJSON r)
    toJSON (PHash x)                = Object (HM.map toJSON x)
    toJSON (PNumber n)              = Number n

instance ToJSON Resource where
    toJSON r = object [ ("type", String $ r ^. rid . itype)
                      , ("title", String $ r ^. rid . iname)
                      , ("aliases", toJSON $ r ^. ralias)
                      , ("exported", Bool $ r ^. rvirtuality == Exported)
                      , ("tags", toJSON $ r ^. rtags)
                      , ("parameters", Object ( HM.map toJSON (r ^. rattributes) `HM.union` relations ))
                      , ("sourceline", r ^. rpos . _1 . to sourceLine . to toJSON)
                      , ("sourcefile", r ^. rpos . _1 . to sourceName . to toJSON)
                      ]
        where
            relations = r ^. rrelations & HM.fromListWith (V.++) . concatMap changeRelations . HM.toList & HM.map toValue
            toValue v | V.length v == 1 = V.head v
                      | otherwise = Array v
            changeRelations :: (RIdentifier, HS.HashSet LinkType) -> [(Text, V.Vector Value)]
            changeRelations (k,v) = do
                c <- HS.toList v
                return (rel2text c, V.singleton (String (rid2text k)))
            rid2text :: RIdentifier -> Text
            rid2text (RIdentifier t n) = capitalizeRT t `T.append` "[" `T.append` capn `T.append` "]"
                where
                    capn = if t == "classe"
                             then capitalizeRT n
                             else n

instance FromJSON Resource where
    parseJSON (Object v) = do
        isExported <- v .: "exported"
        let virtuality = if isExported
                             then Exported
                             else Normal
            getResourceIdentifier :: PValue -> Maybe RIdentifier
            getResourceIdentifier (PString x) =
                let (restype, brckts) = T.breakOn "[" x
                    rna | T.null brckts        = Nothing
                        | T.null restype       = Nothing
                        | T.last brckts == ']' = Just (T.tail (T.init brckts))
                        | otherwise            = Nothing
                in case rna of
                       Just resname -> Just (RIdentifier (T.toLower restype) (T.toLower resname))
                       _ -> Nothing
            getResourceIdentifier _ = Nothing
            -- TODO : properly handle metaparameters
            separate :: (Container PValue, HM.HashMap RIdentifier (HS.HashSet LinkType)) -> Text -> PValue -> (Container PValue, HM.HashMap RIdentifier (HS.HashSet LinkType))
            separate (curAttribs, curRelations) k val = case (fromJSON (String k), getResourceIdentifier val) of
                                                           (Success rel, Just ri) -> (curAttribs, curRelations & at ri . non mempty . contains rel .~ True)
                                                           _                 -> (curAttribs & at k ?~ val, curRelations)
        (attribs,relations) <- HM.foldlWithKey' separate (mempty,mempty) <$> v .: "parameters"
        contimport <- v .:? "certname" .!= "unknown"
        Resource
                <$> (RIdentifier <$> fmap T.toLower (v .: "type") <*> v .: "title")
                <*> v .:? "aliases" .!= mempty
                <*> pure attribs
                <*> pure relations
                <*> pure [ContImport contimport ContRoot]
                <*> pure virtuality
                <*> v .: "tags"
                <*> (toPPos <$> v .:? "sourcefile" .!= "null" <*> v .:? "sourceline" .!= 0)
                <*> pure contimport

    parseJSON _ = mempty

instance ToJSON a => ToJSON (Query a) where
    toJSON (QOr qs)          = toJSON ("or" : map toJSON qs)
    toJSON (QAnd qs)         = toJSON ("and" : map toJSON qs)
    toJSON (QNot q)          = toJSON [ "not" , toJSON q ]
    toJSON (QEqual flds val) = toJSON [ "=",  toJSON flds, toJSON val ]
    toJSON (QMatch flds val) = toJSON [ "~",  toJSON flds, toJSON val ]
    toJSON (QL     flds val) = toJSON [ "<",  toJSON flds, toJSON val ]
    toJSON (QG     flds val) = toJSON [ ">",  toJSON flds, toJSON val ]
    toJSON (QLE    flds val) = toJSON [ "<=", toJSON flds, toJSON val ]
    toJSON (QGE    flds val) = toJSON [ ">=", toJSON flds, toJSON val ]
    toJSON (QEmpty)          = Null

instance ToJSON a => ToText (Query a) where
    toText = T.decodeUtf8 . Control.Lens.view strict . encode

instance FromJSON a => FromJSON (Query a) where
    parseJSON Null = pure QEmpty
    parseJSON (Array elems) = case V.toList elems of
      ("or":xs)          -> QOr    <$> mapM parseJSON xs
      ("and":xs)         -> QAnd   <$> mapM parseJSON xs
      ["not",x]          -> QNot   <$> parseJSON x
      [ "=", flds, val ] -> QEqual <$> parseJSON flds    <*> parseJSON val
      [ "~", flds, val ] -> QEqual <$> parseJSON flds    <*> parseJSON val
      [ ">", flds, val ] -> QG     <$> parseJSON flds    <*> parseJSON val
      [ "<", flds, val ] -> QL     <$> parseJSON flds    <*> parseJSON val
      [">=", flds, val ] -> QGE    <$> parseJSON flds    <*> parseJSON val
      ["<=", flds, val ] -> QLE    <$> parseJSON flds    <*> parseJSON val
      x -> fail ("unknown query" ++ show x)
    parseJSON _ = fail "Expected an array"

instance ToJSON FactField where
    toJSON FName     = "name"
    toJSON FValue    = "value"
    toJSON FCertname = "certname"

instance FromJSON FactField where
    parseJSON "name"     = pure FName
    parseJSON "value"    = pure FValue
    parseJSON "certname" = pure FCertname
    parseJSON _          = fail "Can't parse fact field"

instance ToJSON NodeField where
    toJSON NName = "name"
    toJSON (NFact t) = toJSON [ "fact", t ]

instance FromJSON NodeField where
    parseJSON (Array xs) = case V.toList xs of
                               ["fact", x] -> NFact <$> parseJSON x
                               _ -> fail "Invalid field syntax"
    parseJSON (String "name") = pure NName
    parseJSON _ = fail "invalid field"

instance ToJSON ResourceField where
    toJSON RTag           = "tag"
    toJSON RCertname      = "certname"
    toJSON (RParameter t) = toJSON ["parameter", t]
    toJSON RType          = "type"
    toJSON RTitle         = "title"
    toJSON RExported      = "exported"
    toJSON RFile          = "file"
    toJSON RLine          = "line"

instance FromJSON ResourceField where
    parseJSON (Array xs) = case V.toList xs of
                               ["parameter", x] -> RParameter <$> parseJSON x
                               _ -> fail "Invalid field syntax"
    parseJSON (String "tag"     ) = pure RTag
    parseJSON (String "certname") = pure RCertname
    parseJSON (String "type"    ) = pure RType
    parseJSON (String "title"   ) = pure RTitle
    parseJSON (String "exported") = pure RExported
    parseJSON (String "file"    ) = pure RFile
    parseJSON (String "line"    ) = pure RLine
    parseJSON _ = fail "invalid field"

instance FromJSON RIdentifier where
    parseJSON (Object v) = RIdentifier <$> v .: "type" <*> v .: "title"
    parseJSON _ = fail "invalid resource"

instance ToJSON RIdentifier where
    toJSON (RIdentifier t n) = object [("type", String t), ("title", String n)]

instance FromJSON PuppetEdge where
    parseJSON (Object v) = PuppetEdge <$> v .: "source" <*> v .: "target" <*> v .: "relationship"
    parseJSON _ = fail "invalid puppet edge"

instance ToJSON PuppetEdge where
    toJSON (PuppetEdge s t r) = object [("source", toJSON s), ("target", toJSON t), ("relationship", toJSON r)]

instance FromJSON WireCatalog where
    parseJSON (Object d) = d .: "data" >>= \case
        (Object v) -> WireCatalog
                <$> v .: "name"
                <*> v .: "version"
                <*> v .: "edges"
                <*> v .: "resources"
                <*> v .: "transaction-uuid"
        _ -> fail "Data is not an object"
    parseJSON _ = fail "invalid wire catalog"

instance ToJSON WireCatalog where
    toJSON (WireCatalog n v e r t) = object [("metadata", object [("api_version", Number 1)]), ("data", object d)]
        where d = [ ("name", String n)
                  , ("version", String v)
                  , ("edges", toJSON e)
                  , ("resources", toJSON r)
                  , ("transaction-uuid", String t)
                  ]

instance ToJSON FactInfo where
    toJSON (FactInfo n f v) = object [("certname", String n), ("name", String f), ("value", toJSON v)]

instance FromJSON FactInfo where
    parseJSON (Object v) = FactInfo <$> v .: "certname" <*> v .: "name" <*> v .: "value"
    parseJSON _ = fail "invalid fact info"

instance ToJSON NodeInfo where
    toJSON p = object [ ("name"             , toJSON (p ^. nodeInfoName))
                      , ("deactivated"      , toJSON (p ^. nodeInfoDeactivated))
                      , ("catalog_timestamp", toJSON (p ^. nodeInfoCatalogT))
                      , ("facts_timestamp"  , toJSON (p ^. nodeInfoFactsT))
                      , ("report_timestamp" , toJSON (p ^. nodeInfoReportT))
                      ]

instance FromJSON NodeInfo where
    parseJSON (Object v) = NodeInfo <$> v .:  "name"
                                     <*> v .:? "deactivated" .!= False
                                     <*> v .:  "catalog_timestamp"
                                     <*> v .:  "facts_timestamp"
                                     <*> v .:  "report_timestamp"
    parseJSON _ = fail "invalide node info"
