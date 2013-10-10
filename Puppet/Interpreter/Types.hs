{-# LANGUAGE DeriveGeneric, TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Puppet.Interpreter.Types where

import Puppet.Parser.Types
import Puppet.Stats
import Puppet.Parser.PrettyPrinter
import Text.Parsec.Pos

import Data.Aeson
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Tuple.Strict
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Monad.Error
import Control.Lens
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

#ifdef HRUBY
import Foreign.Ruby
#endif

metaparameters :: HS.HashSet T.Text
metaparameters = HS.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"]

type Container = HM.HashMap T.Text

data PValue = PBoolean !Bool
            | PUndef
            | PString !T.Text -- integers and doubles are internally serialized as strings by puppet
            | PResourceReference !T.Text !T.Text
            | PArray !(V.Vector PValue)
            | PHash !(Container PValue)
            deriving (Eq, Show)

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

data ResDefaults = ResDefaults { _defType     :: !T.Text
                               , _defSrcScope :: !T.Text
                               , _defValues   :: !(Container PValue)
                               , _defPos      :: !PPosition
                               }

data CurContainerDesc = ContRoot | ContClass !T.Text | ContDefine !T.Text !T.Text
    deriving Eq

data CurContainer = CurContainer { _cctype :: !CurContainerDesc
                                 , _cctags :: !(HS.HashSet T.Text)
                                 }
                                 deriving Eq

data ResRefOverride = ResRefOverride { _rrid     :: !RIdentifier
                                     , _rrparams :: !(Container PValue)
                                     , _rrpos    :: !PPosition
                                     }
                                     deriving Eq

data ScopeInformation = ScopeInformation { _scopeVariables :: !(Container (Pair PValue PPosition))
                                         , _scopeDefaults  :: !(Container ResDefaults)
                                         , _scopeExtraTags :: !(HS.HashSet T.Text)
                                         , _scopeContainer :: !CurContainer
                                         , _scopeOverrides :: !(HM.HashMap RIdentifier ResRefOverride)
                                         , _scopeParent    :: !(S.Maybe T.Text)
                                         }

data InterpreterState = InterpreterState { _scopes             :: !(Container ScopeInformation)
                                         , _loadedClasses      :: !(Container (Pair ClassIncludeType PPosition))
                                         , _definedResources   :: !(HM.HashMap RIdentifier PPosition)
                                         , _curScope           :: ![Scope]
                                         , _curPos             :: !PPosition
                                         , _nestedDeclarations :: !(HM.HashMap (TopLevelType, T.Text) Statement)
                                         , _extraRelations     :: ![LinkInformation]
                                         , _resMod             :: ![ResourceModifier]
                                         }

data InterpreterReader = InterpreterReader { _nativeTypes             :: !(Container PuppetTypeMethods)
                                           , _getStatement            :: TopLevelType -> T.Text -> IO (S.Either Doc Statement)
                                           , _computeTemplateFunction :: Either T.Text T.Text -> T.Text -> Container ScopeInformation -> IO (S.Either Doc T.Text)
                                           , _puppetDBquery           :: T.Text -> Value -> IO (S.Either String Value)
                                           , _externalFunctions       :: Container ( [PValue] -> InterpreterMonad PValue )
                                           }

data Warning = Warning !Doc

data InterpreterWriter = InterpreterWriter { _warnings :: ![Pair Priority Doc] }

warn :: Doc -> InterpreterMonad ()
warn d = tell (InterpreterWriter [WARNING :!: d])

logWriter :: Priority -> Doc -> InterpreterMonad ()
logWriter prio d = tell (InterpreterWriter [prio :!: d])

instance Monoid InterpreterWriter where
    mempty = InterpreterWriter []
    mappend (InterpreterWriter a) (InterpreterWriter b) = {-# SCC "mappendInterpreterWriter" #-} InterpreterWriter (a ++ b)

type InterpreterMonad = ErrorT Doc (RWST InterpreterReader InterpreterWriter InterpreterState IO)

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
                  | CantReplace

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
    , _ralias      :: !T.Text                                         -- ^ All the resource aliases
    , _rattributes :: !(Container PValue)                             -- ^ Resource parameters.
    , _rrelations  :: !(HM.HashMap RIdentifier (HS.HashSet LinkType)) -- ^ Resource relations.
    , _rscope      :: !T.Text                                         -- ^ Resource scope when it was defined
    , _rcontainer  :: !CurContainerDesc                               -- ^ The class that contains this resource
    , _rvirtuality :: !Virtuality
    , _rtags       :: !(HS.HashSet T.Text)
    , _rpos        :: !PPosition -- ^ Source code position of the resource definition.
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

throwPosError :: Doc -> InterpreterMonad a
throwPosError s = use (curPos . _1) >>= \p -> throwError (s <+> "at" <+> showPos p)

getScope :: InterpreterMonad Scope
{-# INLINE getScope #-}
getScope = use curScope >>= \s -> if null s
                                      then throwPosError "Internal error: empty scope!"
                                      else return (head s)

-- instance

instance FromJSON PValue where
    parseJSON Null = return PUndef
    parseJSON (Number n) = return (PString (T.pack (show n)))
    parseJSON (String s) = return (PString s)
    parseJSON (Bool b) = return (PBoolean b)
    parseJSON (Array v) = fmap PArray (V.mapM parseJSON v)
    parseJSON (Object o) = fmap PHash (TR.mapM parseJSON o)

instance ToJSON PValue where
    toJSON (PBoolean b) = Bool b
    toJSON PUndef = Null
    toJSON (PString s) = String s
    toJSON (PResourceReference _ _) = Null -- TODO
    toJSON (PArray r) = Array (V.map toJSON r)
    toJSON (PHash x) = Object (HM.map toJSON x)

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

interpreterIO :: IO (S.Either Doc a) -> InterpreterMonad a
{-# INLINE interpreterIO #-}
interpreterIO f = do
    liftIO (f `catch` (\e -> return $ S.Left $ dullred $ text $ show (e :: SomeException))) >>= \case
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

instance ToJSON Resource where
    toJSON r = object [ ("type", String $ r ^. rid . itype)
                      , ("title", String $ r ^. rid . iname)
                      , ("exported", Bool $ r ^. rvirtuality == Exported)
                      , ("tags", toJSON $ r ^. rtags)
                      , ("parameters", Object (HM.map toJSON $ r ^. rattributes))
                      , ("line", r ^. rpos . _1 . to sourceLine . to toJSON)
                      , ("file", r ^. rpos . _1 . to sourceName . to toJSON)
                      ]

instance FromJSON Resource where
    parseJSON (Object v) = mempty
    parseJSON _ = mempty
