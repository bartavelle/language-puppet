{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Puppet.Interpreter.Types where

import           Puppet.Parser.PrettyPrinter
import           Puppet.Parser.Types
import           Puppet.Stats

import           Control.Applicative          hiding (empty)
import           Control.Concurrent.MVar      (MVar)
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Operational
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Class
import           Data.Aeson                   as A
import           Data.Aeson.Lens
import           Data.Attoparsec.Text         (parseOnly, rational)
import qualified Data.ByteString              as BS
import qualified Data.Either.Strict           as S
import qualified Data.Foldable                as F
import           Data.Hashable
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Data.Maybe                   (fromMaybe)
import qualified Data.Maybe.Strict            as S
import           Data.Monoid                  hiding ((<>))
import           Data.Scientific
import           Data.String                  (IsString (..))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock
import qualified Data.Traversable             as TR
import           Data.Tuple.Strict
import qualified Data.Vector                  as V
import           Foreign.Ruby
import           GHC.Generics                 hiding (to)
import           GHC.Stack
import qualified Scripting.Lua                as Lua
import           System.Log.Logger
import           Text.Parsec.Pos
import           Text.PrettyPrint.ANSI.Leijen hiding (rational, (<$>))

metaparameters :: HS.HashSet T.Text
metaparameters = HS.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"]

type Nodename = T.Text

type Container = HM.HashMap T.Text

newtype PrettyError = PrettyError { getError :: Doc }

instance Show PrettyError where
    show = show . getError

instance IsString PrettyError where
    fromString = PrettyError . string

-- | The intepreter can run in two modes : a strict mode (recommended), and
-- a permissive mode. The permissive mode let known antipatterns work with
-- the interpreter.
data Strictness = Strict | Permissive
                deriving (Show, Eq)


data PValue = PBoolean !Bool
            | PUndef
            | PString !T.Text -- integers and doubles are internally serialized as strings by puppet
            | PResourceReference !T.Text !T.Text
            | PArray !(V.Vector PValue)
            | PHash !(Container PValue)
            | PNumber !Scientific
            deriving (Eq, Show)

-- | The different kind of hiera queries
data HieraQueryType = Priority   -- ^ standard hiera query
                    | ArrayMerge -- ^ hiera_array
                    | HashMerge  -- ^ hiera_hash

-- | The type of the Hiera API function
type HieraQueryFunc m = Container T.Text -- ^ All the variables that Hiera can interpolate, the top level ones being prefixed with ::
                     -> T.Text -- ^ The query
                     -> HieraQueryType
                     -> m (S.Either PrettyError (Maybe PValue))

data RSearchExpression = REqualitySearch !T.Text !PValue
                       | RNonEqualitySearch !T.Text !PValue
                       | RAndSearch !RSearchExpression !RSearchExpression
                       | ROrSearch !RSearchExpression !RSearchExpression
                       | RAlwaysTrue
                       deriving Eq

instance IsString PValue where
    fromString = PString . T.pack

data ClassIncludeType = IncludeStandard | IncludeResource
                      deriving (Eq)

type Scope = T.Text

type Facts = Container PValue

-- |This type is used to differenciate the distinct top level types that are
-- exposed by the DSL.
data TopLevelType
    -- |This is for node entries.
    = TopNode
    -- |This is for defines.
    | TopDefine
    -- |This is for classes.
    | TopClass
    -- |This one is special. It represents top level statements that are not
    -- part of a node, define or class. It is defined as spurious because it is
    -- not what you are supposed to be. Also the caching system doesn't like
    -- them too much right now.
    | TopSpurious
    deriving (Generic,Eq)

instance Hashable TopLevelType

data ResDefaults = ResDefaults
    { _defType     :: !T.Text
    , _defSrcScope :: !T.Text
    , _defValues   :: !(Container PValue)
    , _defPos      :: !PPosition
    }

data CurContainerDesc = ContRoot -- ^ Contained at node or root level
                      | ContClass !T.Text -- ^ Contained in a class
                      | ContDefine !T.Text !T.Text !PPosition -- ^ Contained in a define, along with the position where this define was ... defined
                      | ContImported !CurContainerDesc -- ^ Dummy container for imported resources, so that we know we must update the nodename
                      | ContImport !Nodename !CurContainerDesc -- ^ This one is used when finalizing imported resources, and contains the current node name
                      deriving (Eq, Generic, Ord)

data CurContainer = CurContainer
    { _cctype :: !CurContainerDesc
    , _cctags :: !(HS.HashSet T.Text)
    } deriving (Eq)

data ResRefOverride = ResRefOverride
    { _rrid     :: !RIdentifier
    , _rrparams :: !(Container PValue)
    , _rrpos    :: !PPosition
    } deriving (Eq)

data ScopeInformation = ScopeInformation
    { _scopeVariables :: !(Container (Pair (Pair PValue PPosition) CurContainerDesc))
    , _scopeDefaults  :: !(Container ResDefaults)
    , _scopeExtraTags :: !(HS.HashSet T.Text)
    , _scopeContainer :: !CurContainer
    , _scopeOverrides :: !(HM.HashMap RIdentifier ResRefOverride)
    , _scopeParent    :: !(S.Maybe T.Text)
    }

data InterpreterState = InterpreterState
    { _scopes             :: !(Container ScopeInformation)
    , _loadedClasses      :: !(Container (Pair ClassIncludeType PPosition))
    , _definedResources   :: !(HM.HashMap RIdentifier Resource)
    , _curScope           :: ![CurContainerDesc]
    , _curPos             :: !PPosition
    , _nestedDeclarations :: !(HM.HashMap (TopLevelType,T.Text) Statement)
    , _extraRelations     :: ![LinkInformation]
    , _resMod             :: ![ResourceModifier]
    }

data InterpreterReader m = InterpreterReader
    { _nativeTypes             :: !(Container NativeTypeMethods)
    , _getStatement            :: TopLevelType -> T.Text -> m (S.Either PrettyError Statement)
    , _computeTemplateFunction :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> m (S.Either PrettyError T.Text)
    , _pdbAPI                  :: PuppetDBAPI m
    , _externalFunctions       :: Container ([PValue] -> InterpreterMonad PValue)
    , _thisNodename            :: T.Text
    , _hieraQuery              :: HieraQueryFunc m
    , _ioMethods               :: ImpureMethods m
    , _ignoredModules          :: HS.HashSet T.Text -- ^ The set of modules we will not include or whatsoever.
    , _isStrict                :: Bool -- ^ Are we running in strict mode ?
    }

data ImpureMethods m = ImpureMethods
    { _imGetCurrentCallStack :: m [String]
    , _imReadFile            :: [T.Text] -> m (Either String T.Text)
    , _imTraceEvent          :: String -> m ()
    , _imCallLua             :: MVar Lua.LuaState -> T.Text -> [PValue] -> m (Either String PValue)
    }

data InterpreterInstr a where
    -- Utility for using what's in "InterpreterReader"
    GetNativeTypes      :: InterpreterInstr (Container NativeTypeMethods)
    GetStatement        :: TopLevelType -> T.Text -> InterpreterInstr Statement
    ComputeTemplate     :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> InterpreterInstr T.Text
    ExternalFunction    :: T.Text -> [PValue] -> InterpreterInstr PValue
    GetNodeName         :: InterpreterInstr T.Text
    HieraQuery          :: Container T.Text -> T.Text -> HieraQueryType -> InterpreterInstr (Maybe PValue)
    GetCurrentCallStack :: InterpreterInstr [String]
    IsIgnoredModule     :: T.Text -> InterpreterInstr Bool
    IsStrict            :: InterpreterInstr Bool
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
    PDBReplaceFacts     :: [(Nodename, Facts)] -> InterpreterInstr ()
    PDBDeactivateNode   :: Nodename -> InterpreterInstr ()
    PDBGetFacts         :: Query FactField -> InterpreterInstr [PFactInfo]
    PDBGetResources     :: Query ResourceField -> InterpreterInstr [Resource]
    PDBGetNodes         :: Query NodeField -> InterpreterInstr [PNodeInfo]
    PDBCommitDB         :: InterpreterInstr ()
    PDBGetResourcesOfNode :: Nodename -> Query ResourceField -> InterpreterInstr [Resource]
    -- Reading the first file that can be read in a list
    ReadFile            :: [T.Text] -> InterpreterInstr T.Text
    -- Tracing events
    TraceEvent          :: String -> InterpreterInstr ()
    -- Calling Lua
    CallLua             :: MVar Lua.LuaState -> T.Text -> [PValue] -> InterpreterInstr PValue

newtype Warning = Warning Doc

type InterpreterLog = Pair Priority Doc
type InterpreterWriter = [InterpreterLog]

warn :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
warn d = tell [WARNING :!: d]

debug :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
debug d = tell [DEBUG :!: d]

logWriter :: (Monad m, MonadWriter InterpreterWriter m) => Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

-- | The main monad
type InterpreterMonad = ProgramT InterpreterInstr (State InterpreterState)

instance MonadError PrettyError InterpreterMonad where
    throwError = singleton . ErrorThrow
    catchError a c = singleton (ErrorCatch a c)

instance MonadWriter InterpreterWriter InterpreterMonad where
    tell = singleton . WriterTell
    pass = singleton . WriterPass
    listen = singleton . WriterListen

instance Error PrettyError where
    noMsg = PrettyError empty
    strMsg = PrettyError . text

data RIdentifier = RIdentifier
    { _itype :: !T.Text
    , _iname :: !T.Text
    } deriving (Show,Eq,Generic,Ord)

instance Hashable RIdentifier

data ModifierType = ModifierCollector -- ^ For collectors, optional resources
                  | ModifierMustMatch -- ^ For stuff like realize
                  deriving Eq

data OverrideType = CantOverride -- ^ Overriding forbidden, will throw an error
                  | Replace -- ^ Can silently replace
                  | CantReplace -- ^ Silently ignore errors

data ResourceCollectorType = RealizeVirtual
                           | RealizeCollected
                           | DontRealize
                           deriving Eq


data ResourceModifier = ResourceModifier
    { _rmResType      :: !T.Text
    , _rmModifierType :: !ModifierType
    , _rmType         :: !ResourceCollectorType
    , _rmSearch       :: !RSearchExpression
    , _rmMutation     :: !(Resource -> InterpreterMonad Resource)
    , _rmDeclaration  :: !PPosition
    }

data LinkInformation = LinkInformation
    { _linksrc  :: !RIdentifier
    , _linkdst  :: !RIdentifier
    , _linkType :: !LinkType
    , _linkPos  :: !PPosition
    }

type EdgeMap = HM.HashMap RIdentifier [LinkInformation]

{-| A fully resolved puppet resource that will be used in the 'FinalCatalog'. -}
data Resource = Resource
    { _rid         :: !RIdentifier                                    -- ^ Resource name.
    , _ralias      :: !(HS.HashSet T.Text)                            -- ^ All the resource aliases
    , _rattributes :: !(Container PValue)                             -- ^ Resource parameters.
    , _rrelations  :: !(HM.HashMap RIdentifier (HS.HashSet LinkType)) -- ^ Resource relations.
    , _rscope      :: ![CurContainerDesc]                             -- ^ Resource scope when it was defined, the real container will be the first item
    , _rvirtuality :: !Virtuality
    , _rtags       :: !(HS.HashSet T.Text)
    , _rpos        :: !PPosition -- ^ Source code position of the resource definition.
    , _rnode       :: !Nodename -- ^ The node were this resource was created, if remote
    }
    deriving Eq

type NativeTypeValidate = Resource -> Either PrettyError Resource

-- | Attributes (and providers) of a puppet resource type bundled with validation rules
data NativeTypeMethods = NativeTypeMethods
    { _puppetValidate :: NativeTypeValidate
    , _puppetFields   :: HS.HashSet T.Text
    }

type FinalCatalog = HM.HashMap RIdentifier Resource

data DaemonMethods = DaemonMethods
    { -- | The most important function, computing catalogs.
      -- Given a node name and a list of facts, it returns the result of the catalog compilation : either an error, or a tuple containing all the resources in this catalog, the dependency map, the exported resources, and a list of known resources, that might not be up to date, but are here for code coverage tests.
      _dGetCatalog    :: Nodename -> Facts -> IO (S.Either PrettyError (FinalCatalog, EdgeMap, FinalCatalog, [Resource]))
    , _dParserStats   :: MStats
    , _dCatalogStats  :: MStats
    , _dTemplateStats :: MStats
    }

data PuppetEdge = PuppetEdge RIdentifier RIdentifier LinkType

-- | Wire format, see <http://docs.puppetlabs.com/puppetdb/1.5/api/wire_format/catalog_format.html>.
data WireCatalog = WireCatalog
    { _wireCatalogNodename        :: !Nodename
    , _wireCatalogWVersion        :: !T.Text
    , _wireCatalogWEdges          :: !(V.Vector PuppetEdge)
    , _wireCatalogWResources      :: !(V.Vector Resource)
    , _wireCatalogTransactionUUID :: !T.Text
    }

data PFactInfo = PFactInfo
    { _pFactInfoNodename :: !T.Text
    , _pFactInfoFactname :: !T.Text
    , _pFactInfoFactval  :: !PValue
    }

data PNodeInfo = PNodeInfo
    { _pNodeInfoNodename    :: !Nodename
    , _pNodeInfoDeactivated :: !Bool
    , _pNodeInfoCatalogT    :: !(S.Maybe UTCTime)
    , _pNodeInfoFactsT      :: !(S.Maybe UTCTime)
    , _pNodeInfoReportT     :: !(S.Maybe UTCTime)
    }

data PuppetDBAPI m = PuppetDBAPI
    { pdbInformation     :: m Doc
    , replaceCatalog     :: WireCatalog         -> m (S.Either PrettyError ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-catalog-version-3>
    , replaceFacts       :: [(Nodename, Facts)] -> m (S.Either PrettyError ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-facts-version-1>
    , deactivateNode     :: Nodename            -> m (S.Either PrettyError ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#deactivate-node-version-1>
    , getFacts           :: Query FactField     -> m (S.Either PrettyError [PFactInfo]) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/facts.html#get-v3facts>
    , getResources       :: Query ResourceField -> m (S.Either PrettyError [Resource]) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/resources.html#get-v3resources>
    , getNodes           :: Query NodeField     -> m (S.Either PrettyError [PNodeInfo])
    , commitDB           ::                        m (S.Either PrettyError ()) -- ^ This is only here to tell the test PuppetDB to save its content to disk.
    , getResourcesOfNode :: Nodename -> Query ResourceField -> m (S.Either PrettyError [Resource])
    }

-- | Pretty straightforward way to define the various PuppetDB queries
data Query a = QEqual a T.Text
             | QG a Integer
             | QL a Integer
             | QGE a Integer
             | QLE a Integer
             | QMatch T.Text T.Text
             | QAnd [Query a]
             | QOr [Query a]
             | QNot (Query a)
             | QEmpty

-- | Fields for the fact endpoint
data FactField = FName
               | FValue
               | FCertname

-- | Fields for the node endpoint
data NodeField = NName | NFact T.Text

-- | Fields for the resource endpoint
data ResourceField = RTag
                   | RCertname
                   | RParameter T.Text
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
makeClassy ''DaemonMethods
makeClassy ''NativeTypeMethods
makeClassy ''ScopeInformation
makeClassy ''Resource
makeClassy ''InterpreterState
makeClassy ''InterpreterReader
makeClassy ''ImpureMethods
makeClassy ''CurContainer
makeFields ''WireCatalog
makeFields ''PFactInfo
makeFields ''PNodeInfo

rcurcontainer :: Resource -> CurContainerDesc
rcurcontainer r = fromMaybe ContRoot (r ^? rscope . _head)

class MonadThrowPos m where
    throwPosError :: Doc -> m a

class MonadStack m where
    getCallStack :: m [String]

instance MonadStack InterpreterMonad where
    getCallStack = singleton GetCurrentCallStack

instance MonadThrowPos InterpreterMonad where
    throwPosError s = do
        p <- use (curPos . _1)
        stack <- getCallStack
        let dstack = if null stack
                         then mempty
                         else mempty </> string (renderStack stack)
        throwError (PrettyError (s <+> "at" <+> showPos p <> dstack))

getCurContainer :: InterpreterMonad CurContainer
{-# INLINABLE getCurContainer #-}
getCurContainer = do
    scp <- getScopeName
    preuse (scopes . ix scp . scopeContainer) >>= \case
        Just x -> return x
        Nothing -> throwPosError ("Internal error: can't find the current container for" <+> green (string (T.unpack scp)))

scopeName :: CurContainerDesc -> T.Text
scopeName (ContRoot        ) = "::"
scopeName (ContImported x  ) = "::imported::" `T.append` scopeName x
scopeName (ContClass x     ) = x
scopeName (ContDefine dt dn _) = "#define/" `T.append` dt `T.append` "/" `T.append` dn
scopeName (ContImport _ x  ) = "::import::" `T.append` scopeName x

getScopeName :: InterpreterMonad T.Text
getScopeName = fmap scopeName getScope

getScope :: InterpreterMonad CurContainerDesc
{-# INLINABLE getScope #-}
getScope = use curScope >>= \s -> if null s
                                      then throwPosError "Internal error: empty scope!"
                                      else return (head s)

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

instance ToRuby PValue where
    toRuby = toRuby . toJSON
instance FromRuby PValue where
    fromRuby v = fromRuby v >>= \case
            Nothing -> return Nothing
            Just x  -> case fromJSON x of
                           Error _ -> return Nothing
                           Success suc -> return (Just suc)

eitherDocIO :: IO (S.Either PrettyError a) -> IO (S.Either PrettyError a)
eitherDocIO computation = (computation >>= check) `catch` (\e -> return $ S.Left $ PrettyError $ dullred $ text $ show (e :: SomeException))
    where
        check (S.Left r) = return (S.Left r)
        check (S.Right x) = return (S.Right x)

interpreterIO :: (MonadThrowPos m, MonadIO m) => IO (S.Either PrettyError a) -> m a
{-# INLINABLE interpreterIO #-}
interpreterIO f =
    liftIO (eitherDocIO f) >>= \case
        S.Right x -> return x
        S.Left rr -> throwPosError (getError rr)

mightFail :: (MonadError PrettyError m, MonadThrowPos m) => m (S.Either PrettyError a) -> m a
mightFail a = a >>= \case
    S.Right x -> return x
    S.Left rr -> throwPosError (getError rr)

safeDecodeUtf8 :: BS.ByteString -> InterpreterMonad T.Text
{-# INLINABLE safeDecodeUtf8 #-}
safeDecodeUtf8 i = return (T.decodeUtf8 i)

interpreterError :: InterpreterMonad (S.Either PrettyError a) -> InterpreterMonad a
{-# INLINABLE interpreterError #-}
interpreterError f = f >>= \case
                             S.Right r -> return r
                             S.Left rr -> throwPosError (getError rr)

resourceRelations :: Resource -> [(RIdentifier, LinkType)]
resourceRelations = concatMap expandSet . HM.toList . _rrelations
    where
        expandSet (ri, lts) = [(ri, lt) | lt <- HS.toList lts]

-- | helper for hashmap, in case we want another kind of map ..
ifromList :: (Monoid m, At m, F.Foldable f) => f (Index m, IxValue m) -> m
{-# INLINABLE ifromList #-}
ifromList = F.foldl' (\curm (k,v) -> curm & at k ?~ v) mempty

ikeys :: (Eq k, Hashable k) => HM.HashMap k v -> HS.HashSet k
{-# INLINABLE ikeys #-}
ikeys = HS.fromList . HM.keys

isingleton :: (Monoid b, At b) => Index b -> IxValue b -> b
{-# INLINABLE isingleton #-}
isingleton k v = mempty & at k ?~ v

ifromListWith :: (Monoid m, At m, F.Foldable f) => (IxValue m -> IxValue m -> IxValue m) -> f (Index m, IxValue m) -> m
{-# INLINABLE ifromListWith #-}
ifromListWith f = F.foldl' (\curmap (k,v) -> iinsertWith f k v curmap) mempty

iinsertWith :: At m => (IxValue m -> IxValue m -> IxValue m) -> Index m -> IxValue m -> m -> m
{-# INLINABLE iinsertWith #-}
iinsertWith f k v m = m & at k %~ mightreplace
    where
        mightreplace Nothing = Just v
        mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
{-# INLINABLE iunionWith #-}
iunionWith = HM.unionWith

fnull :: (Eq x, Monoid x) => x -> Bool
{-# INLINABLE fnull #-}
fnull = (== mempty)

rid2text :: RIdentifier -> T.Text
rid2text (RIdentifier t n) = capitalizeRT t `T.append` "[" `T.append` capn `T.append` "]"
    where
        capn = if t == "classe"
                   then capitalizeRT n
                   else n

instance ToJSON Resource where
    toJSON r = object [ ("type", String $ r ^. rid . itype)
                      , ("title", String $ r ^. rid . iname)
                      , ("aliases", toJSON $ r ^. ralias)
                      , ("exported", Bool $ r ^. rvirtuality == Exported)
                      , ("tags", toJSON $ r ^. rtags)
                      , ("parameters", Object ( (HM.map toJSON $ r ^. rattributes) `HM.union` relations ))
                      , ("sourceline", r ^. rpos . _1 . to sourceLine . to toJSON)
                      , ("sourcefile", r ^. rpos . _1 . to sourceName . to toJSON)
                      ]
        where
            relations = r ^. rrelations & HM.fromListWith (V.++) . concatMap changeRelations . HM.toList & HM.map toValue
            toValue v | V.length v == 1 = V.head v
                      | otherwise = Array v
            changeRelations :: (RIdentifier, HS.HashSet LinkType) -> [(T.Text, V.Vector Value)]
            changeRelations (k,v) = do
                c <- HS.toList v
                return (rel2text c, V.singleton (String (rid2text k)))

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
            separate :: (Container PValue, HM.HashMap RIdentifier (HS.HashSet LinkType)) -> T.Text -> PValue -> (Container PValue, HM.HashMap RIdentifier (HS.HashSet LinkType))
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

instance ToJSON PFactInfo where
    toJSON (PFactInfo n f v) = object [("certname", String n), ("name", String f), ("value", toJSON v)]

instance FromJSON PFactInfo where
    parseJSON (Object v) = PFactInfo <$> v .: "certname" <*> v .: "name" <*> v .: "value"
    parseJSON _ = fail "invalid fact info"

instance ToJSON PNodeInfo where
    toJSON p = object [ ("name"             , toJSON (p ^. nodename))
                      , ("deactivated"      , toJSON (p ^. deactivated))
                      , ("catalog_timestamp", toJSON (p ^. catalogT))
                      , ("facts_timestamp"  , toJSON (p ^. factsT))
                      , ("report_timestamp" , toJSON (p ^. reportT))
                      ]

instance FromJSON PNodeInfo where
    parseJSON (Object v) = PNodeInfo <$> v .:  "name"
                                     <*> v .:? "deactivated" .!= False
                                     <*> v .:  "catalog_timestamp"
                                     <*> v .:  "facts_timestamp"
                                     <*> v .:  "report_timestamp"
    parseJSON _ = fail "invalide node info"

text2Scientific :: T.Text -> Maybe Scientific
text2Scientific t = case parseOnly rational t of
            Left _ -> Nothing
            Right s -> Just s

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

initialState :: Facts -> InterpreterState
initialState facts = InterpreterState baseVars initialclass mempty [ContRoot] dummypos mempty [] []
    where
        callervars = HM.fromList [("caller_module_name", PString "::" :!: dummypos :!: ContRoot), ("module_name", PString "::" :!: dummypos :!: ContRoot)]
        factvars = facts & each %~ (\x -> x :!: initialPPos "facts" :!: ContRoot)
        baseVars = HM.singleton "::" (ScopeInformation (factvars `mappend` callervars) mempty mempty (CurContainer ContRoot mempty) mempty S.Nothing)
        initialclass = mempty & at "::" ?~ (IncludeStandard :!: dummypos)

dummypos :: PPosition
dummypos = initialPPos "dummy"

-- | Throws an error if we are in strict mode
checkStrict :: Doc -- ^ The warning message.
            -> Doc -- ^ The error message.
            -> InterpreterMonad ()
checkStrict wrn err = do
    str <- singleton IsStrict
    if str
        then throwPosError err
        else warn wrn

-- | Runs operations depending on the strict flag.
ifStrict :: InterpreterMonad a -- ^ This operation will be run in strict mode.
         -> InterpreterMonad a -- ^ This operation will be run in permissive mode.
         -> InterpreterMonad a
ifStrict yes no = do
    str <- singleton IsStrict
    if str then yes else no
