module Puppet.Init where

import System.IO
import Puppet.Interpreter.Types
import qualified Data.Map as Map

data Prefs = Prefs {
    manifest :: FilePath,
    modules :: FilePath,
    templates :: FilePath,
    compilepoolsize :: Int,
    parsepoolsize :: Int
} deriving (Show)

genPrefs :: String -> Prefs
genPrefs basedir = Prefs (basedir ++ "/manifests") (basedir ++ "/modules") (basedir ++ "/templates") 1 1

genFacts :: [(String,String)] -> Facts
genFacts = Map.fromList . concatMap (\(a,b) -> [(a, ResolvedString b), ("::" ++ a, ResolvedString b)])

