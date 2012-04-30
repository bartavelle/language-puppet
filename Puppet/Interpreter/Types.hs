module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import qualified Data.Map as Map

type Catalog =[CResource]
type Facts = Map.Map String Value

data LinkType = RNotify | RRequire | RBefore | RRegister deriving(Show)

data ResolvedValue
    = SimpleParamString String
    | SimpleParamArray [ResolvedValue]
    | SimpleParamHash [(String, ResolvedValue)]
    deriving(Show)

data CResource = CResource {
    crid :: Int,
    crname :: String,
    crtype :: String,
    crparams :: [(String, ResolvedValue)],
	relations :: [(LinkType, String, String)], -- (relation, resname, resname)
    crvirtuality :: Virtuality,
    filename :: String,
    fileline :: Int
    } deriving(Show)

