module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import Text.Parsec.Pos
import qualified Data.Map as Map

type Catalog =[CResource]
type Facts = Map.Map String ResolvedValue

data LinkType = RNotify | RRequire | RBefore | RRegister deriving(Show)

data ResolvedValue
    = ResolvedParamString String
    | ResolvedParamArray [ResolvedValue]
    | ResolvedParamHash [(String, ResolvedValue)]
    deriving(Show)

data CResource = CResource {
    crid :: Int,
    crname :: String,
    crtype :: String,
    crparams :: [(String, Either Expression ResolvedValue)],
	relations :: [(LinkType, String, String)], -- (relation, resname, resname)
    crvirtuality :: Virtuality,
    pos :: SourcePos
    } deriving(Show)

