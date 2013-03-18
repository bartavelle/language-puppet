module Puppet.Interpreter.Types where

import Puppet.DSL.Types hiding (Value) -- conflicts with aeson
import Puppet.Utils

import qualified PuppetDB.Query as PDB
import qualified Scripting.Lua as Lua
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exts
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Number
import qualified Text.Parsec.Pos as TPP
import qualified Data.Vector as V

-- | Types for the native type system.
type PuppetTypeName = T.Text
-- |This is a function type than can be bound. It is the type of all subsequent
-- validators.
type PuppetTypeValidate = RResource -> Either String RResource
data PuppetTypeMethods = PuppetTypeMethods {
    puppetvalidate :: PuppetTypeValidate,
    puppetfields   :: Set.Set T.Text
    }

-- | This is the potentially unsolved list of resources in the catalog.
type Catalog =[CResource]
type Facts = Map.Map T.Text ResolvedValue

-- | Relationship link type.
data LinkType = RNotify | RRequire | RBefore | RSubscribe deriving(Show, Ord, Eq)
type LinkInfo = (LinkType, RelUpdateType, SourcePos, [[ScopeName]])

-- | The list of resolved values that are used to define everything in a
-- 'FinalCatalog' and in the resolved parts of a 'Catalog'. They are to be
-- compared with the 'Value's.
data ResolvedValue
    = ResolvedString     !T.Text
    | ResolvedRegexp     !T.Text
    | ResolvedInt        !Integer
    | ResolvedDouble     !Double
    | ResolvedBool       !Bool
    | ResolvedRReference !T.Text !ResolvedValue
    | ResolvedArray      ![ResolvedValue]
    | ResolvedHash       ![(T.Text, ResolvedValue)]
    | ResolvedUndefined
    deriving(Show, Eq, Ord)

instance ToJSON ResolvedValue where
    toJSON (ResolvedString s)       = String s
    toJSON (ResolvedRegexp r)       = String r
    toJSON (ResolvedInt i)          = Number (I i)
    toJSON (ResolvedDouble d)       = Number (D d)
    toJSON (ResolvedBool b)         = Bool b
    toJSON (ResolvedRReference _ _) = Null -- TODO
    toJSON (ResolvedArray rr)       = toJSON rr
    toJSON (ResolvedHash hh)        = object (map (\(t,v) -> t .= v) hh)
    toJSON (ResolvedUndefined)      = Null

parseResourceReference :: T.Text -> Maybe ResolvedValue
parseResourceReference instr = case T.splitOn "[" instr of
                                               [restype, renamee] -> if T.last renamee == ']'
                                                                         then Just (ResolvedRReference (T.toLower restype) (ResolvedString (T.init renamee)))
                                                                         else Nothing
                                               _ -> Nothing

instance FromJSON ResolvedValue where
    parseJSON Null = return ResolvedUndefined
    parseJSON (Number x) = return $ case x of
                                        (I n) -> ResolvedInt n
                                        (D d) -> ResolvedDouble d
    parseJSON (String s) = case parseResourceReference s of
                               Just x  -> return x
                               Nothing -> return $ ResolvedString s
    parseJSON (Array a) = fmap ResolvedArray (mapM parseJSON (V.toList a))
    parseJSON (Object o) = fmap ResolvedHash (mapM (\(a,b) -> do {
                                                                 b' <- parseJSON b ;
                                                                 return (a,b') }
                                                                 ) (HM.toList o))
    parseJSON (Bool b) = return $ ResolvedBool b


-- | This type holds a value that is either from the ASL or fully resolved.
type GeneralValue = Either Expression ResolvedValue
-- | This type holds a value that is either from the ASL or a fully resolved
-- String.
type GeneralString = Either Expression T.Text

{-| This describes the resources before the final resolution. This is required
as they must somehow be collected while the 'Statement's are interpreted, but
the necessary 'Expression's are not yet available. This is because in Puppet the
'Statement' order should not alter the catalog's content.

The relations are not stored here, as they are pushed into a separate internal
data structure by the interpreter.
-}
data CResource = CResource {
    crid :: !Int, -- ^ Resource ID, used in the Puppet YAML.
    crname :: !GeneralString, -- ^ Resource name.
    crtype :: !T.Text, -- ^ Resource type.
    crparams :: !(Map.Map GeneralString GeneralValue), -- ^ Resource parameters.
    crvirtuality :: !Virtuality, -- ^ Resource virtuality.
    crscope :: ![[ScopeName]], -- ^ Resource scope when it was defined
    pos :: !SourcePos -- ^ Source code position of the resource definition.
    } deriving(Show)

instance FromJSON CResource where
    parseJSON (Object o) = do
        utitle     <- o .:  "title"
        params     <- o .:  "parameters"
        sourcefile <- o .:  "sourcefile"
        sourceline <- o .:  "sourceline"
        certname   <- o .:  "certname"
        scope      <- o .:? "scope"
        let _ = params :: HM.HashMap T.Text ResolvedValue
            parameters = Map.fromList $ map (\(k,v) -> (Right k, Right v)) $ ("EXPORTEDSOURCE", ResolvedString certname) : HM.toList params :: Map.Map GeneralString GeneralValue
            position   = TPP.newPos (T.unpack sourcefile ++ "(host: " ++ T.unpack certname ++ ")") sourceline 1
            mscope = case scope of
                         Just x  -> x
                         Nothing -> [["json"]]
        CResource <$> pure 0
                  <*> pure (Right utitle)
                  <*> fmap T.toLower (o .: "type")
                  <*> pure parameters
                  <*> pure Normal
                  <*> pure mscope
                  <*> pure position
    parseJSON _ = mzero


-- | Used for puppetDB queries, converting values to CResources
json2puppet :: (FromJSON a) => Value -> Either String a
json2puppet x = case fromJSON x of
                         Error s   -> Left s
                         Success a -> Right a

-- | Resource identifier, made of a type, name pair.
type ResIdentifier = (T.Text, T.Text)

-- | Resource relation, made of a 'LinkType', 'ResIdentifier' pair.
type Relation  = (LinkType, ResIdentifier)

{-| This is a fully resolved resource that will be used in the 'FinalCatalog'.
-}
data RResource = RResource {
    rrid :: !Int, -- ^ Resource ID.
    rrname :: !T.Text, -- ^ Resource name.
    rrtype :: !T.Text, -- ^ Resource type.
    rrparams :: !(Map.Map T.Text ResolvedValue), -- ^ Resource parameters.
	rrelations :: ![Relation], -- ^ Resource relations.
    rrscope :: ![[ScopeName]], -- ^ Resource scope when it was defined
    rrpos :: !SourcePos -- ^ Source code position of the resource definition.
    } deriving(Show, Ord, Eq)

rr2json :: T.Text -> RResource -> Value
rr2json hostname rr =
    let sourcefile = sourceName (rrpos rr)
        sourceline = sourceLine (rrpos rr)
    in  object [ "title"      .= rrname rr
               , "sourcefile" .= sourcefile
               , "sourceline" .= sourceline
               , "type"       .= capitalizeResType (rrtype rr)
               , "certname"   .= hostname
               , "scope"      .= rrscope rr
               , "parameters" .= rrparams rr
               ]

type FinalCatalog = Map.Map ResIdentifier RResource

type ScopeName = T.Text

-- | Type of update\/override, so they can be applied in the correct order. This
-- part is probably not behaving like vanilla puppet, as it turns out this are
-- many fairly acceptable behaviours and the correct one is not documented.
data RelUpdateType = UNormal | UOverride | UDefault | UPlus deriving (Show, Ord, Eq)

{-| A data type to hold defaults values
 -}
data ResDefaults = RDefaults T.Text (Map.Map GeneralString GeneralValue) SourcePos
                 | ROverride T.Text GeneralString (Map.Map GeneralString GeneralValue) SourcePos
                 deriving (Show, Ord, Eq)

{-| The most important data structure for the interpreter. It stores its
internal state.
-}
data ScopeState = ScopeState {
    curScope :: ![[ScopeName]],
    -- ^ The list of scopes. It works like a stack, and its initial value must
    -- be @[[\"::\"]]@. It is a stack of lists of strings. These lists can be
    -- one element wide (usual case), or two elements (inheritance), so that
    -- variables could be assigned to both scopes.
    curVariables :: !(Map.Map T.Text (GeneralValue, SourcePos)),
    -- ^ The list of known variables. It should be noted that the interpreter
    -- tries to resolve them as soon as it can, so that it can store their
    -- current scope.
    curClasses :: !(Map.Map T.Text SourcePos),
    -- ^ The list of classes that have already been included, along with the
    -- place where this happened.
    curDefaults :: !(Map.Map [ScopeName] [ResDefaults]),
    -- ^ List of defaults to apply. All defaults are applied at the end of the
    -- interpretation of each top level statement.
    curResId :: !Int, -- ^ Stores the value of the current 'crid'.
    curPos :: !SourcePos,
    -- ^ Current position of the evaluated statement. This is mostly used to
    -- give useful error messages.
    nestedtoplevels :: !(Map.Map (TopLevelType, T.Text) Statement),
    -- ^ List of \"top levels\" that have been parsed inside another top level.
    -- Their behaviour is curently non canonical as the scoping rules are
    -- unclear.
    getStatementsFunction :: TopLevelType -> T.Text -> IO (Either String Statement),
    -- ^ This is a function that, given the type of a top level statement and
    -- its name, should return it.
    getWarnings :: ![T.Text], -- ^ List of warnings.
    curCollect :: ![(CResource -> CatalogMonad Bool, Map.Map GeneralString GeneralValue, Maybe PDB.Query)],
    -- ^ A bit complicated, this stores the collection functions. These are
    -- functions that determine whether a resource should be collected or not.
    -- It can optionally store overrides, which will be applied in the end on
    -- all resources. It can also store a PuppetDB query.
    unresolvedRels :: ![([(LinkType, GeneralValue, GeneralValue)], (T.Text, GeneralString), RelUpdateType, SourcePos, [[ScopeName]])],
    -- ^ This stores unresolved relationships, because the original string name
    -- can't be resolved. Fieds are [ ( [dstrelations], srcresource, type, pos ) ]
    computeTemplateFunction :: T.Text -> T.Text -> Map.Map T.Text GeneralValue -> IO (Either String T.Text),
    -- ^ Function that takes a filename, the current scope and a list of
    -- variables. It returns an error or the computed template.
    puppetDBFunction :: T.Text -> PDB.Query -> IO (Either String Value),
    -- ^ Function that takes a request type (resources, nodes, facts, ..),
    -- a query, and returns a resolved value from puppetDB.
    luaState :: Maybe Lua.LuaState,
    -- ^ The Lua state, used for user supplied content.
    userFunctions :: !(Set.Set T.Text),
    -- ^ The list of registered user functions
    nativeTypes :: !(Map.Map PuppetTypeName PuppetTypeMethods),
    -- ^ The list of native types.
    definedResources :: !(Map.Map ResIdentifier SourcePos),
    -- ^ Horrible hack to kind of support the "defined" function
    currentDependencyStack :: [ResIdentifier]
}

-- | The monad all the interpreter lives in. It is 'ErrorT' with a state.
type CatalogMonad = ErrorT T.Text (StateT ScopeState IO)

instance Error T.Text where
    noMsg = ""
    strMsg = T.pack

-- | This is the map of all edges associated with the 'FinalCatalog'.
-- The key is (source, target).
type EdgeMap = Map.Map (ResIdentifier, ResIdentifier) LinkInfo

generalizeValueE :: Expression -> GeneralValue
generalizeValueE = Left
generalizeValueR :: ResolvedValue -> GeneralValue
generalizeValueR = Right
generalizeStringE :: Expression -> GeneralString
generalizeStringE = Left
generalizeStringS :: T.Text -> GeneralString
generalizeStringS = Right

-- |This is the set of meta parameters
metaparameters = Set.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"] :: Set.Set T.Text

getPos               = liftM curPos get
modifyScope     f sc = sc { curScope       = f $ curScope sc }
modifyDeps      f sc = sc { currentDependencyStack = f $ currentDependencyStack sc }
modifyVariables f sc = sc { curVariables   = f $ curVariables sc }
modifyClasses   f sc = sc { curClasses     = f $ curClasses sc }
incrementResId    sc = sc { curResId       = curResId sc + 1 }
setStatePos  npos sc = sc { curPos         = npos }
pushWarning     t sc = sc { getWarnings    = getWarnings sc ++ [t] }
pushCollect   r   sc = sc { curCollect     = r : curCollect sc }
pushUnresRel  r   sc = sc { unresolvedRels = r : unresolvedRels sc }
addDefinedResource r p = modify (\st -> st { definedResources = Map.insert r p (definedResources st) } )
saveVariables vars = modify (\st -> st { curVariables = vars })

showScope :: [[ScopeName]] -> T.Text
showScope = tshow . reverse . concat . map (take 1)

throwPosError :: T.Text -> CatalogMonad a
throwPosError msg = do
    p <- getPos
    st <- fmap (map T.pack) (liftIO currentCallStack)
    throwError (msg <> " at " <> tshow p <> "\n\t" <> T.intercalate "\n\t" st)

addAlias :: T.Text -> RResource -> Either String RResource
addAlias value res = case Map.lookup "alias" (rrparams res) of
                          Nothing                   -> Right $! insertparam res "alias" (ResolvedArray [ResolvedString value])
                          Just a@(ResolvedString _) -> Right $! insertparam res "alias" (ResolvedArray [a,ResolvedString value])
                          Just (ResolvedArray ar)   -> Right $! insertparam res "alias" (ResolvedArray (ResolvedString value : ar))
                          Just x                    -> Left ("Aliases should be strings or arrays of strings, not " ++ show x)

insertparam :: RResource -> T.Text -> ResolvedValue -> RResource
insertparam res param value = res { rrparams = Map.insert param value (rrparams res) }

