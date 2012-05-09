module Puppet.Interpreter.Types where

import Puppet.DSL.Types
import Text.Parsec.Pos
import qualified Data.Map as Map

type Catalog =[CResource]
type Facts = Map.Map String ResolvedValue

data LinkType = RNotify | RRequire | RBefore | RRegister deriving(Show)

data ResolvedValue
    = ResolvedString String
    | ResolvedInt   Integer
    | ResolvedBool Bool
    | ResolvedRReference String ResolvedValue
    | ResolvedArray [ResolvedValue]
    | ResolvedHash [(String, ResolvedValue)]
    deriving(Show, Eq)

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

generalizeValueE :: Expression -> GeneralValue
generalizeValueE e = Left e
generalizeValueR :: ResolvedValue -> GeneralValue
generalizeValueR e = Right e
generalizeStringE :: Expression -> GeneralString
generalizeStringE s = Left s
generalizeStringS :: String -> GeneralString
generalizeStringS s = Right s

