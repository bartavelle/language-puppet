module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map

type Catalog =[CResource]
type Facts = Map.Map String ResolvedValue

data LinkType = RNotify | RRequire | RBefore | RRegister deriving(Show, Ord, Eq)

data ResolvedValue
    = ResolvedString String
    | ResolvedInt   Integer
    | ResolvedBool Bool
    | ResolvedRReference String ResolvedValue
    | ResolvedArray [ResolvedValue]
    | ResolvedHash [(String, ResolvedValue)]
    | ResolvedUndefined
    deriving(Show, Eq, Ord)

type GeneralValue = Either Expression ResolvedValue
type GeneralString = Either Expression String

data CResource = CResource {
    crid :: Int,
    crname :: GeneralString,
    crtype :: String,
    crparams :: [(GeneralString, GeneralValue)],
    crvirtuality :: Virtuality,
    pos :: SourcePos
    } deriving(Show)

type ResIdentifier = (String, String) -- type, name

type Relation  = (LinkType, ResIdentifier) -- (relation, dst)

data RResource = RResource {
    rrid :: Int,
    rrname :: String,
    rrtype :: String,
    rrparams :: Map.Map String ResolvedValue,
	rrelations :: [Relation],
    rrpos :: SourcePos
    } deriving(Show, Ord, Eq)
    

type FinalCatalog = Map.Map ResIdentifier RResource

type ScopeName = String

data RelUpdateType = UNormal | UOverride | UDefault | UPlus deriving (Show, Ord, Eq)

data ScopeState = ScopeState {
    curScope :: [ScopeName],
    curVariables :: Map.Map String (GeneralValue, SourcePos),
    curClasses :: Map.Map String SourcePos,
    curDefaults :: [Statement],
    curResId :: Int,
    curPos :: SourcePos,
    netstedtoplevels :: Map.Map (TopLevelType, String) Statement,
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement),
    getWarnings :: [String],
    -- this stores the collection functions
    curCollect :: [CResource -> CatalogMonad Bool],
    -- this stores unresolved relationships, because the original string name can't be resolved
    -- fieds are [ ( [dstrelations], srcresource, type, pos ) ]
    unresolvedRels :: [([(LinkType, GeneralValue, GeneralValue)], (String, GeneralString), RelUpdateType, SourcePos)],
    computeTemplateFunction :: String -> String -> [(String, GeneralValue)] -> IO (Either String String)
}

type CatalogMonad = ErrorT String (StateT ScopeState IO)

generalizeValueE :: Expression -> GeneralValue
generalizeValueE e = Left e
generalizeValueR :: ResolvedValue -> GeneralValue
generalizeValueR e = Right e
generalizeStringE :: Expression -> GeneralString
generalizeStringE s = Left s
generalizeStringS :: String -> GeneralString
generalizeStringS s = Right s

