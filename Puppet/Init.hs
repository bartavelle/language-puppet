{-| This is a helper module for the "Puppet.Daemon" module -}
module Puppet.Init where

import Puppet.Interpreter.Types
import qualified Data.Map as Map

data Prefs = Prefs {
    manifest :: FilePath, -- ^ The path to the manifests.
    modules :: FilePath, -- ^ The path to the modules.
    templates :: FilePath, -- ^ The path to the template.
    compilepoolsize :: Int, -- ^ Size of the compiler pool.
    parsepoolsize :: Int -- ^ Size of the parser pool.
} deriving (Show)

-- | Generates the 'Prefs' structure from a single path.
--
-- > genPrefs "/etc/puppet"
genPrefs :: String -> Prefs
genPrefs basedir = Prefs (basedir ++ "/manifests") (basedir ++ "/modules") (basedir ++ "/templates") 1 1

-- | Generates 'Facts' from pairs of strings.
--
-- > genFacts [("hostname","test.com")]
genFacts :: [(String,String)] -> Facts
genFacts = Map.fromList . concatMap (\(a,b) -> [(a, ResolvedString b), ("::" ++ a, ResolvedString b)])

