{-# LANGUAGE OverloadedStrings #-}

module Puppet.Interpreter.Types where

import Puppet.DSL.Types hiding (Value,Error) -- conflicts with aeson

import qualified PuppetDB.Query as PDB
import qualified Scripting.Lua as Lua
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Exts
import Data.List
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Number
import qualified Text.Parsec.Pos as TPP
import qualified Data.Vector as V
import Data.Char (toLower)

-- | Types for the native type system.
type PuppetTypeName = String
-- |This is a function type than can be bound. It is the type of all subsequent
-- validators.
type PuppetTypeValidate = RResource -> Either String RResource
data PuppetTypeMethods = PuppetTypeMethods {
    puppetvalidate :: PuppetTypeValidate,
    puppetfields   :: Set.Set String
    }

-- | This is the potentially unsolved list of resources in the catalog.
type Catalog =[CResource]
type Facts = Map.Map String ResolvedValue

-- | Relationship link type.
data LinkType = RNotify | RRequire | RBefore | RSubscribe deriving(Show, Ord, Eq)
type LinkInfo = (LinkType, RelUpdateType, SourcePos)

-- | The list of resolved values that are used to define everything in a
-- 'FinalCatalog' and in the resolved parts of a 'Catalog'. They are to be
-- compared with the 'Value's.
data ResolvedValue
    = ResolvedString     !String
    | ResolvedRegexp     !String
    | ResolvedInt        !Integer
    | ResolvedDouble     !Double
    | ResolvedBool       !Bool
    | ResolvedRReference !String !ResolvedValue
    | ResolvedArray      ![ResolvedValue]
    | ResolvedHash       ![(String, ResolvedValue)]
    | ResolvedUndefined
    deriving(Show, Eq, Ord)

parseResourceReference :: T.Text -> Maybe ResolvedValue
parseResourceReference instr = case break (=='[') (T.unpack instr) of
                                              (restype, '[':renamee) -> if (last renamee == ']')
                                                                            then Just (ResolvedRReference (map toLower restype) (ResolvedString (init renamee)))
                                                                            else Nothing
                                              _ -> Nothing

instance FromJSON ResolvedValue where
    parseJSON Null = return ResolvedUndefined
    parseJSON (Number x) = return $ case x of
                                        (I n) -> ResolvedInt n
                                        (D d) -> ResolvedDouble d
    parseJSON (String s) = case parseResourceReference s of
                               Just x  -> return x
                               Nothing -> return $ ResolvedString $ T.unpack s
    parseJSON (Array a) = fmap ResolvedArray (mapM parseJSON (V.toList a))
    parseJSON (Object o) = fmap ResolvedHash (mapM (\(a,b) -> do {
                                                                 b' <- parseJSON b ;
                                                                 return (T.unpack a,b') }
                                                                 ) (HM.toList o))
    parseJSON (Bool b) = return $ ResolvedBool b


-- | This type holds a value that is either from the ASL or fully resolved.
type GeneralValue = Either Expression ResolvedValue
-- | This type holds a value that is either from the ASL or a fully resolved
-- String.
type GeneralString = Either Expression String

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
    crtype :: !String, -- ^ Resource type.
    crparams :: !(Map.Map GeneralString GeneralValue), -- ^ Resource parameters.
    crvirtuality :: !Virtuality, -- ^ Resource virtuality.
    pos :: !SourcePos -- ^ Source code position of the resource definition.
    } deriving(Show)

instance FromJSON CResource where
    parseJSON (Object o) = do
        utitle     <- o .: "title"
        params     <- o .: "parameters"
        sourcefile <- o .: "sourcefile"
        sourceline <- o .: "sourceline"
        certname   <- o .: "certname"
        let _ = params :: HM.HashMap String ResolvedValue
            parameters = Map.fromList $ map (\(k,v) -> (Right k, Right v)) $ ("EXPORTEDSOURCE", ResolvedString certname) : HM.toList params :: Map.Map GeneralString GeneralValue
            position   = TPP.newPos (sourcefile ++ "(host: " ++ certname ++ ")") sourceline 1
        CResource <$> pure 0
                  <*> pure (Right utitle)
                  <*> fmap (T.unpack . T.toLower) (o .: "type")
                  <*> pure parameters
                  <*> pure Normal
                  <*> pure position
    parseJSON _ = mzero


-- | Used for puppetDB queries, converting values to CResources
value2CResources :: Value -> Either String [CResource]
value2CResources Null = Right []
value2CResources x = case fromJSON x of
                         Error s   -> Left s
                         Success a -> Right a

-- | Resource identifier, made of a type, name pair.
type ResIdentifier = (String, String)

-- | Resource relation, made of a 'LinkType', 'ResIdentifier' pair.
type Relation  = (LinkType, ResIdentifier)

{-| This is a fully resolved resource that will be used in the 'FinalCatalog'.
-}
data RResource = RResource {
    rrid :: !Int, -- ^ Resource ID.
    rrname :: !String, -- ^ Resource name.
    rrtype :: !String, -- ^ Resource type.
    rrparams :: !(Map.Map String ResolvedValue), -- ^ Resource parameters.
	rrelations :: ![Relation], -- ^ Resource relations.
    rrpos :: !SourcePos -- ^ Source code position of the resource definition.
    } deriving(Show, Ord, Eq)


type FinalCatalog = Map.Map ResIdentifier RResource

type ScopeName = String

-- | Type of update\/override, so they can be applied in the correct order. This
-- part is probably not behaving like vanilla puppet, as it turns out this are
-- many fairly acceptable behaviours and the correct one is not documented.
data RelUpdateType = UNormal | UOverride | UDefault | UPlus deriving (Show, Ord, Eq)

{-| A data type to hold defaults values
 -}
data ResDefaults = RDefaults String (Map.Map GeneralString GeneralValue) SourcePos
                 | ROverride String GeneralString (Map.Map GeneralString GeneralValue) SourcePos
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
    curVariables :: !(Map.Map String (GeneralValue, SourcePos)),
    -- ^ The list of known variables. It should be noted that the interpreter
    -- tries to resolve them as soon as it can, so that it can store their
    -- current scope.
    curClasses :: !(Map.Map String SourcePos),
    -- ^ The list of classes that have already been included, along with the
    -- place where this happened.
    curDefaults :: !(Map.Map [ScopeName] [ResDefaults]),
    -- ^ List of defaults to apply. All defaults are applied at the end of the
    -- interpretation of each top level statement.
    curResId :: !Int, -- ^ Stores the value of the current 'crid'.
    curPos :: !SourcePos,
    -- ^ Current position of the evaluated statement. This is mostly used to
    -- give useful error messages.
    nestedtoplevels :: !(Map.Map (TopLevelType, String) Statement),
    -- ^ List of \"top levels\" that have been parsed inside another top level.
    -- Their behaviour is curently non canonical as the scoping rules are
    -- unclear.
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement),
    -- ^ This is a function that, given the type of a top level statement and
    -- its name, should return it.
    getWarnings :: ![String], -- ^ List of warnings.
    curCollect :: ![(CResource -> CatalogMonad Bool, Map.Map GeneralString GeneralValue, Maybe PDB.Query)],
    -- ^ A bit complicated, this stores the collection functions. These are
    -- functions that determine whether a resource should be collected or not.
    -- It can optionally store overrides, which will be applied in the end on
    -- all resources. It can also store a PuppetDB query.
    unresolvedRels :: ![([(LinkType, GeneralValue, GeneralValue)], (String, GeneralString), RelUpdateType, SourcePos)],
    -- ^ This stores unresolved relationships, because the original string name
    -- can't be resolved. Fieds are [ ( [dstrelations], srcresource, type, pos ) ]
    computeTemplateFunction :: String -> String -> Map.Map String GeneralValue -> IO (Either String String),
    -- ^ Function that takes a filename, the current scope and a list of
    -- variables. It returns an error or the computed template.
    puppetDBFunction :: String -> PDB.Query -> IO (Either String Value),
    -- ^ Function that takes a request type (resources, nodes, facts, ..),
    -- a query, and returns a resolved value from puppetDB.
    luaState :: Maybe Lua.LuaState,
    -- ^ The Lua state, used for user supplied content.
    userFunctions :: !(Set.Set String),
    -- ^ The list of registered user functions
    nativeTypes :: !(Map.Map PuppetTypeName PuppetTypeMethods),
    -- ^ The list of native types.
    definedResources :: !(Map.Map ResIdentifier SourcePos),
    -- ^ Horrible hack to kind of support the "defined" function
    currentDependencyStack :: [ResIdentifier]
}

-- | The monad all the interpreter lives in. It is 'ErrorT' with a state.
type CatalogMonad = ErrorT String (StateT ScopeState IO)

-- | This is the map of all edges associated with the 'FinalCatalog'.
-- The key is (source, target).
type EdgeMap = Map.Map (ResIdentifier, ResIdentifier) LinkInfo

generalizeValueE :: Expression -> GeneralValue
generalizeValueE = Left
generalizeValueR :: ResolvedValue -> GeneralValue
generalizeValueR = Right
generalizeStringE :: Expression -> GeneralString
generalizeStringE = Left
generalizeStringS :: String -> GeneralString
generalizeStringS = Right

-- |This is the set of meta parameters
metaparameters = Set.fromList ["tag","stage","name","title","alias","audit","check","loglevel","noop","schedule", "EXPORTEDSOURCE", "require", "before", "register", "notify"] :: Set.Set String

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

throwPosError :: String -> CatalogMonad a
throwPosError msg = do
    p <- getPos
    st <- liftIO currentCallStack
    throwError (msg ++ " at " ++ show p ++ intercalate "\n\t" st )


