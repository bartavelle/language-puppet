{-| This is a helper module for the "Puppet.Daemon" module -}
module Puppet.Init where

import Puppet.Interpreter.Types
import Puppet.NativeTypes
import Puppet.NativeTypes.Helpers
import Puppet.Plugins
import qualified PuppetDB.Query as PDB
import Puppet.Utils

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Text as T

data Prefs = Prefs {
    manifest        :: T.Text, -- ^ The path to the manifests.
    modules         :: T.Text, -- ^ The path to the modules.
    templates       :: T.Text, -- ^ The path to the template.
    compilepoolsize :: Int, -- ^ Size of the compiler pool.
    parsepoolsize   :: Int, -- ^ Size of the parser pool.
    erbpoolsize     :: Int, -- ^ Size of the template pool.
    puppetDBquery   :: T.Text -> PDB.Query -> IO (Either String Value), -- ^ A function that takes a query type, a query and might return stuff
    natTypes        :: Map.Map PuppetTypeName PuppetTypeMethods -- ^ The list of native types.
}

-- | Generates the 'Prefs' structure from a single path.
--
-- > genPrefs "/etc/puppet"
genPrefs :: T.Text -> IO Prefs
genPrefs basedir = do
    let manifestdir = basedir <> "/manifests"
        modulesdir  = basedir <> "/modules"
        templatedir = basedir <> "/templates"
    typenames <- fmap (map takeBaseName) (getFiles modulesdir "lib/puppet/type" ".rb")
    let loadedTypes = Map.fromList (map defaulttype typenames)
        cstpdb :: T.Text -> PDB.Query -> IO (Either String Value)
        cstpdb _ _ = return (Right (Array V.empty))
    return $ Prefs manifestdir modulesdir templatedir 1 1 1 cstpdb (Map.union baseNativeTypes loadedTypes)

-- | Generates 'Facts' from pairs of strings.
--
-- > genFacts [("hostname","test.com")]
genFacts :: [(T.Text,T.Text)] -> Facts
genFacts = Map.fromList . concatMap (\(a,b) -> [(a, ResolvedString b), ("::" <> a, ResolvedString b)])

