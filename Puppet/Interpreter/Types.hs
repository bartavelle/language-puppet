module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import Text.Parsec.Pos
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Set as Set

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
    deriving(Show, Eq, Ord)

type GeneralValue = Either Expression ResolvedValue
type GeneralString = Either Expression String

data CResource = CResource {
    crid :: Int,
    crname :: GeneralString,
    crtype :: String,
    crparams :: [(GeneralString, GeneralValue)],
	relations :: [(LinkType, GeneralValue, GeneralValue)], -- (relation, resname, resname)
    crvirtuality :: Virtuality,
    pos :: SourcePos
    } deriving(Show)

type ResIdentifier = (String, String) -- type, name

type Relation  = (LinkType, ResIdentifier, ResIdentifier) -- (relation, src, dst)

data RResource = RResource {
    rrid :: Int,
    rrname :: String,
    rrtype :: String,
    rrparams :: [(String, ResolvedValue)],
	rrelations :: [Relation],
    rrpos :: SourcePos
    } deriving(Show, Ord, Eq)
    

type FinalCatalog = Map.Map ResIdentifier RResource

nativetypes = Set.fromList (["augeas","computer","cron","exec","file","filebucket","group","host","interface","k5login","macauthorization","mailalias","maillist","mcx","mount","nagios_command","nagios_contact","nagios_contactgroup","nagios_host","nagios_hostdependency","nagios_hostescalation","nagios_hostextinfo","nagios_hostgroup","nagios_service","nagios_servicedependency","nagios_serviceescalation","nagios_serviceextinfo","nagios_servicegroup","nagios_timeperiod","notify","package","resources","router","schedule","scheduledtask","selboolean","selmodule","service","sshauthorizedkey","sshkey","stage","tidy","user","vlan","yumrepo","zfs","zone","zpool"] ++ ["class", "ssh_authorized_key_secure"])

type ScopeName = String

data ScopeState = ScopeState {
    curScope :: [ScopeName],
    curVariables :: Map.Map String (GeneralValue, SourcePos),
    curClasses :: Map.Map String SourcePos,
    curDefaults :: [Statement],
    curResId :: Int,
    curPos :: SourcePos,
    netstedtoplevels :: Map.Map (TopLevelType, String) Statement,
    getStatementsFunction :: TopLevelType -> String -> IO (Either String Statement),
    getWarnings :: [String]
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

