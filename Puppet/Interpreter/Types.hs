module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map

-- | This is the potentially unsolved list of resources in the catalog.
type Catalog =[CResource]
type Facts = Map.Map String ResolvedValue

-- | Relationship link type.
data LinkType = RNotify | RRequire | RBefore | RRegister deriving(Show, Ord, Eq)

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
    crid :: Int, -- ^ Resource ID, used in the Puppet YAML.
    crname :: GeneralString, -- ^ Resource name.
    crtype :: String, -- ^ Resource type.
    crparams :: [(GeneralString, GeneralValue)], -- ^ Resource parameters.
    crvirtuality :: Virtuality, -- ^ Resource virtuality.
    pos :: SourcePos -- ^ Source code position of the resource definition.
    } deriving(Show)

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

{-| The most important data structure for the interpreter. It stores its
internal state.
-}
data ScopeState = ScopeState {
    curScope :: ![ScopeName],
    -- ^ The list of scopes. It works like a stack, and its initial value must
    -- be @[\"::\"]@
    curVariables :: !(Map.Map String (GeneralValue, SourcePos)),
    -- ^ The list of known variables. It should be noted that the interpreter
    -- tries to resolve them as soon as it can, so that it can store their
    -- current scope.
    curClasses :: !(Map.Map String SourcePos),
    -- ^ The list of classes that have already been included, along with the
    -- place where this happened.
    curDefaults :: ![Statement],
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
    curCollect :: ![CResource -> CatalogMonad Bool],
    -- ^ A bit complicated, this stores the collection functions. These are
    -- functions that determine whether a resource should be collected or not.
    unresolvedRels :: ![([(LinkType, GeneralValue, GeneralValue)], (String, GeneralString), RelUpdateType, SourcePos)],
    -- ^ This stores unresolved relationships, because the original string name
    -- can't be resolved. Fieds are [ ( [dstrelations], srcresource, type, pos ) ]
    computeTemplateFunction :: String -> String -> [(String, GeneralValue)] -> IO (Either String String)
    -- ^ Function that takes a filename, the current scope and a list of
    -- variables. It returns an error or the computed template.
}

-- | The monad all the interpreter lives in. It is 'ErrorT' with a state.
type CatalogMonad = ErrorT String (StateT ScopeState IO)

generalizeValueE :: Expression -> GeneralValue
generalizeValueE = Left
generalizeValueR :: ResolvedValue -> GeneralValue
generalizeValueR = Right
generalizeStringE :: Expression -> GeneralString
generalizeStringE = Left
generalizeStringS :: String -> GeneralString
generalizeStringS = Right

