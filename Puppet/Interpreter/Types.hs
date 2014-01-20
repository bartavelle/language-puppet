{-# LANGUAGE DeriveGeneric, TemplateHaskell, CPP, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, LambdaCase, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Interpreter.Types where

import Puppet.Parser.Types
import Puppet.Stats
import Puppet.Parser.PrettyPrinter
import Text.Parsec.Pos

import Data.Aeson as A
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Tuple.Strict
import Control.Monad.Trans.RSS.Strict
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Error
import Control.Lens
import Control.Lens.Aeson
import Data.String (IsString(..))
import qualified Data.Either.Strict as S
import qualified Data.Maybe.Strict as S
import Data.Hashable
import GHC.Generics hiding (to)
import qualified Data.Traversable as TR
import Control.Exception
import qualified Data.ByteString as BS
import System.Log.Logger
import Data.List (foldl')
import Control.Applicative hiding (empty)
import Data.Time.Clock
import GHC.Stack
import Data.Maybe (fromMaybe)
import Data.Attoparsec.Number
import Data.Attoparsec.Text (parseOnly,number)

#ifdef HRUBY
import Foreign.Ruby
#endif

metaparameters :: HS.HashSet T.Text
metaparameters = HS.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"]

type Nodename = T.Text

type Container = HM.HashMap T.Text

data PValue = PBoolean !Bool
            | PUndef
            | PString !T.Text -- integers and doubles are internally serialized as strings by puppet
            | PResourceReference !T.Text !T.Text
            | PArray !(V.Vector PValue)
            | PHash !(Container PValue)
            deriving (Eq, Show)

-- | The different kind of hiera queries
data HieraQueryType = Priority   -- ^ standard hiera query
                    | ArrayMerge -- ^ hiera_array
                    | HashMerge  -- ^ hiera_hash

-- | The type of the Hiera API function
type HieraQueryFunc = Container ScopeInformation -> T.Text -> HieraQueryType -> IO (S.Either Doc (Pair InterpreterWriter (S.Maybe PValue)))

data RSearchExpression
    = REqualitySearch !T.Text !PValue
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

type Facts = Container T.Text

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

data ResDefaults = ResDefaults { _defType     :: !T.Text
                               , _defSrcScope :: !T.Text
                               , _defValues   :: !(Container PValue)
                               , _defPos      :: !PPosition
                               }

data CurContainerDesc = ContRoot -- ^ Contained at node or root level
                      | ContClass !T.Text -- ^ Contained in a class
                      | ContDefine !T.Text !T.Text -- ^ Contained in a define
                      | ContImported !CurContainerDesc -- ^ Dummy container for imported resources, so that we know we must update the nodename
                      | ContImport !Nodename !CurContainerDesc -- ^ This one is used when finalizing imported resources, and contains the current node name
    deriving (Eq, Generic, Ord)

data CurContainer = CurContainer { _cctype :: !CurContainerDesc
                                 , _cctags :: !(HS.HashSet T.Text)
                                 }
                                 deriving Eq

data ResRefOverride = ResRefOverride { _rrid     :: !RIdentifier
                                     , _rrparams :: !(Container PValue)
                                     , _rrpos    :: !PPosition
                                     }
                                     deriving Eq

data ScopeInformation = ScopeInformation { _scopeVariables :: !(Container (Pair (Pair PValue PPosition) CurContainerDesc))
                                         , _scopeDefaults  :: !(Container ResDefaults)
                                         , _scopeExtraTags :: !(HS.HashSet T.Text)
                                         , _scopeContainer :: !CurContainer
                                         , _scopeOverrides :: !(HM.HashMap RIdentifier ResRefOverride)
                                         , _scopeParent    :: !(S.Maybe T.Text)
                                         }

data InterpreterState = InterpreterState { _scopes             :: !(Container ScopeInformation)
                                         , _loadedClasses      :: !(Container (Pair ClassIncludeType PPosition))
                                         , _definedResources   :: !(HM.HashMap RIdentifier Resource)
                                         , _curScope           :: ![CurContainerDesc]
                                         , _curPos             :: !PPosition
                                         , _nestedDeclarations :: !(HM.HashMap (TopLevelType, T.Text) Statement)
                                         , _extraRelations     :: ![LinkInformation]
                                         , _resMod             :: ![ResourceModifier]
                                         }

data InterpreterReader = InterpreterReader { _nativeTypes             :: !(Container PuppetTypeMethods)
                                           , _getStatement            :: TopLevelType -> T.Text -> IO (S.Either Doc Statement)
                                           , _computeTemplateFunction :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text)
                                           , _pdbAPI                  :: PuppetDBAPI
                                           , _externalFunctions       :: Container ( [PValue] -> InterpreterMonad PValue )
                                           , _thisNodename            :: T.Text
                                           , _hieraQuery              :: HieraQueryFunc
                                           }

newtype Warning = Warning Doc

type InterpreterLog = Pair Priority Doc
type InterpreterWriter = [InterpreterLog]

warn :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
warn d = tell [WARNING :!: d]

debug :: (Monad m, MonadWriter InterpreterWriter m) => Doc -> m ()
debug d = tell [DEBUG :!: d]

logWriter :: (Monad m, MonadWriter InterpreterWriter m) => Priority -> Doc -> m ()
logWriter prio d = tell [prio :!: d]

type InterpreterMonad = ErrorT Doc (RSST InterpreterReader InterpreterWriter InterpreterState IO)

instance Error Doc where
    noMsg = empty
    strMsg = text

data RIdentifier = RIdentifier { _itype :: !T.Text
                               , _iname :: !T.Text
                               } deriving(Show, Eq, Generic, Ord)

instance Hashable RIdentifier

-- | Relationship link type.
data LinkType = RNotify | RRequire | RBefore | RSubscribe deriving(Show, Eq,Generic)
instance Hashable LinkType

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

data ResourceModifier = ResourceModifier { _rmResType      :: !T.Text
                                         , _rmModifierType :: !ModifierType
                                         , _rmType         :: !ResourceCollectorType
                                         , _rmSearch       :: !RSearchExpression
                                         , _rmMutation     :: !(Resource -> InterpreterMonad Resource)
                                         , _rmDeclaration  :: !PPosition
                                         }

data LinkInformation = LinkInformation { _linksrc  :: !RIdentifier
                                       , _linkdst  :: !RIdentifier
                                       , _linkType :: !LinkType
                                       , _linkPos  :: !PPosition
                                       }

type EdgeMap = HM.HashMap RIdentifier [LinkInformation]

{-| This is a fully resolved resource that will be used in the
    'FinalCatalog'.
-}
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

-- |This is a function type than can be bound. It is the type of all
-- subsequent validators.
type PuppetTypeValidate = Resource -> Either Doc Resource

data PuppetTypeMethods = PuppetTypeMethods
    { _puppetValidate :: PuppetTypeValidate
    , _puppetFields   :: HS.HashSet T.Text
    }

type FinalCatalog = HM.HashMap RIdentifier Resource

data DaemonMethods = DaemonMethods { _dGetCatalog    :: T.Text -> Facts -> IO (S.Either Doc (FinalCatalog, EdgeMap, FinalCatalog))
                                   , _dParserStats   :: MStats
                                   , _dCatalogStats  :: MStats
                                   , _dTemplateStats :: MStats
                                   }

data PuppetEdge = PuppetEdge RIdentifier RIdentifier LinkType

-- | Wire format, see <http://docs.puppetlabs.com/puppetdb/1.5/api/wire_format/catalog_format.html>.
data WireCatalog = WireCatalog { _wirecatalogNodename        :: !Nodename
                               , _wirecatalogWVersion        :: !T.Text
                               , _wirecatalogWEdges          :: !(V.Vector PuppetEdge)
                               , _wirecatalogWResources      :: !(V.Vector Resource)
                               , _wirecatalogTransactionUUID :: !T.Text
                               }

data PFactInfo = PFactInfo { _pfactinfoNodename :: !T.Text
                           , _pfactinfoFactname :: !T.Text
                           , _pfactinfoFactval  :: !T.Text
                           }

data PNodeInfo = PNodeInfo { _pnodeinfoNodename    :: !Nodename
                           , _pnodeinfoDeactivated :: !Bool
                           , _pnodeinfoCatalogT    :: !(S.Maybe UTCTime)
                           , _pnodeinfoFactsT      :: !(S.Maybe UTCTime)
                           , _pnodeinfoReportT     :: !(S.Maybe UTCTime)
                           }

data PuppetDBAPI = PuppetDBAPI { pdbInformation   :: IO Doc
                               , replaceCatalog   :: WireCatalog         -> IO (S.Either Doc ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-catalog-version-3>
                               , replaceFacts     :: [(Nodename, Facts)] -> IO (S.Either Doc ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#replace-facts-version-1>
                               , deactivateNode   :: Nodename            -> IO (S.Either Doc ()) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/commands.html#deactivate-node-version-1>
                               , getFacts         :: Query FactField     -> IO (S.Either Doc [PFactInfo]) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/facts.html#get-v3facts>
                               , getResources     :: Query ResourceField -> IO (S.Either Doc [Resource]) -- ^ <http://docs.puppetlabs.com/puppetdb/1.5/api/query/v3/resources.html#get-v3resources>
                               , getNodes         :: Query NodeField     -> IO (S.Either Doc [PNodeInfo])
                               , commitDB         ::                        IO (S.Either Doc ()) -- ^ This is only here to tell the test PuppetDB to save its content to disk.
                               , getResourcesOfNode :: Nodename -> Query ResourceField -> IO (S.Either Doc [Resource])
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
data FactField = FName | FValue | FCertname

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
makeClassy ''PuppetTypeMethods
makeClassy ''ScopeInformation
makeClassy ''Resource
makeClassy ''InterpreterState
makeClassy ''InterpreterReader
makeClassy ''CurContainer
makeFields ''WireCatalog
makeFields ''PFactInfo
makeFields ''PNodeInfo

rcurcontainer :: Resource -> CurContainerDesc
rcurcontainer r = fromMaybe ContRoot (r ^? rscope . _head)

throwPosError :: Doc -> InterpreterMonad a
throwPosError s = do
    p <- use (curPos . _1)
    stack <- liftIO currentCallStack
    let dstack = if null stack
                     then mempty
                     else mempty </> string (renderStack stack)
    throwError (s <+> "at" <+> showPos p <> dstack)

getCurContainer :: InterpreterMonad CurContainer
{-# INLINE getCurContainer #-}
getCurContainer = do
    scp <- getScopeName
    preuse (scopes . ix scp . scopeContainer) >>= \case
        Just x -> return x
        Nothing -> throwPosError ("Internal error: can't find the current container for" <+> green (string (T.unpack scp)))

scopeName :: CurContainerDesc -> T.Text
scopeName (ContRoot        ) = "::"
scopeName (ContImported x  ) = "::imported::" `T.append` scopeName x
scopeName (ContClass x     ) = x
scopeName (ContDefine dt dn) = "#define/" `T.append` dt `T.append` "/" `T.append` dn
scopeName (ContImport _ x  ) = "::import::" `T.append` scopeName x

getScopeName :: InterpreterMonad T.Text
getScopeName = fmap scopeName getScope

getScope :: InterpreterMonad CurContainerDesc
{-# INLINE getScope #-}
getScope = use curScope >>= \s -> if null s
                                      then throwPosError "Internal error: empty scope!"
                                      else return (head s)

-- instance

instance FromJSON PValue where
    parseJSON Null       = return PUndef
    parseJSON (Number n) = return (PString (T.pack (show n)))
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

#ifdef HRUBY
instance ToRuby PValue where
    toRuby = toRuby . toJSON
instance FromRuby PValue where
    fromRuby v = fromRuby v >>= \case
            Nothing -> return Nothing
            Just x  -> case fromJSON x of
                           Error _ -> return Nothing
                           Success suc -> return (Just suc)
#endif

eitherDocIO :: IO (S.Either Doc a) -> IO (S.Either Doc a)
eitherDocIO computation = (computation >>= check) `catch` (\e -> return $ S.Left $ dullred $ text $ show (e :: SomeException))
    where
        check (S.Left r) = return (S.Left r)
        check (S.Right x) = return (S.Right x)

interpreterIO :: IO (S.Either Doc a) -> InterpreterMonad a
{-# INLINE interpreterIO #-}
interpreterIO f = do
    liftIO (eitherDocIO f) >>= \case
        S.Right x -> return x
        S.Left rr -> throwPosError rr

safeDecodeUtf8 :: BS.ByteString -> InterpreterMonad T.Text
{-# INLINE safeDecodeUtf8 #-}
safeDecodeUtf8 i = return (T.decodeUtf8 i)

interpreterError :: InterpreterMonad (S.Either Doc a) -> InterpreterMonad a
{-# INLINE interpreterError #-}
interpreterError f = f >>= \case
                             S.Right r -> return r
                             S.Left rr -> throwPosError rr

resourceRelations :: Resource -> [(RIdentifier, LinkType)]
resourceRelations = concatMap expandSet . HM.toList . _rrelations
    where
        expandSet (ri, lts) = [(ri, lt) | lt <- HS.toList lts]

-- | helper for hashmap, in case we want another kind of map ..
ifromList :: (Monoid m, At m) => [(Index m, IxValue m)] -> m
{-# INLINE ifromList #-}
ifromList = foldl' (\curm (k,v) -> curm & at k ?~ v) mempty

ikeys :: (Eq k, Hashable k) => HM.HashMap k v -> HS.HashSet k
{-# INLINE ikeys #-}
ikeys = HS.fromList . HM.keys

isingleton :: (Monoid b, At b) => Index b -> IxValue b -> b
{-# INLINE isingleton #-}
isingleton k v = mempty & at k ?~ v

ifromListWith :: (Monoid m, At m) => (IxValue m -> IxValue m -> IxValue m) -> [(Index m, IxValue m)] -> m
{-# INLINE ifromListWith #-}
ifromListWith f = foldl' (\curmap (k,v) -> iinsertWith f k v curmap) mempty

iinsertWith :: At m => (IxValue m -> IxValue m -> IxValue m) -> Index m -> IxValue m -> m -> m
{-# INLINE iinsertWith #-}
iinsertWith f k v m = m & at k %~ mightreplace
    where
        mightreplace Nothing = Just v
        mightreplace (Just x) = Just (f v x)

iunionWith :: (Hashable k, Eq k) => (v -> v -> v) -> HM.HashMap k v -> HM.HashMap k v -> HM.HashMap k v
{-# INLINE iunionWith #-}
iunionWith = HM.unionWith

fnull :: (Eq x, Monoid x) => x -> Bool
{-# INLINE fnull #-}
fnull = (== mempty)

rel2text :: LinkType -> T.Text
rel2text RNotify = "notify"
rel2text RRequire = "require"
rel2text RBefore = "before"
rel2text RSubscribe = "subscribe"

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
                return (rel2text c,V.singleton (String (rid2text k)))

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

instance FromJSON LinkType where
    parseJSON (String "require")   = pure RRequire
    parseJSON (String "notify")    = pure RNotify
    parseJSON (String "subscribe") = pure RSubscribe
    parseJSON (String "before")    = pure RBefore
    parseJSON _ = fail "invalid linktype"

instance ToJSON LinkType where
    toJSON = String . rel2text

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
    toJSON (PFactInfo n f v) = object [("certname", String n), ("name", String f), ("value", String v)]

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

instance AsNumber PValue where
    _Number = prism num2PValue toNumber
        where
            num2PValue :: Number -> PValue
            num2PValue (I x) = PString (T.pack (show x))
            num2PValue (D x) = PString (T.pack (show x))
            toNumber :: PValue -> Either PValue Number
            toNumber p@(PString x) = case parseOnly number x of
                                         Right y -> Right y
                                         _       -> Left p
            toNumber p = Left p

