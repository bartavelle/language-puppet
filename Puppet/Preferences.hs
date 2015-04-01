{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Puppet.Preferences (
    setupPreferences
  , HasPreferences(..)
  , Preferences(Preferences)
  , PuppetDirPaths
  , HasPuppetDirPaths(..)
) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           System.Posix               (fileExist)

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes
import           Puppet.NativeTypes.Helpers
import           Puppet.Plugins
import           Puppet.Stdlib
import           Puppet.Utils
import           PuppetDB.Dummy

data PuppetDirPaths = PuppetDirPaths
    { _baseDir       :: FilePath -- ^ Puppet base working directory
    , _manifestPath  :: FilePath -- ^ The path to the manifests.
    , _modulesPath   :: FilePath -- ^ The path to the modules.
    , _templatesPath :: FilePath -- ^ The path to the template.
    , _testPath      :: FilePath -- ^ The path to a tests folders to hold tests files such as the pdbfiles.
    }

makeClassy ''PuppetDirPaths

data Preferences m = Preferences
    { _puppetPaths    :: PuppetDirPaths
    , _prefPDB        :: PuppetDBAPI m
    , _natTypes       :: Container NativeTypeMethods -- ^ The list of native types.
    , _prefExtFuncs   :: Container ( [PValue] -> InterpreterMonad PValue )
    , _hieraPath      :: Maybe FilePath
    , _ignoredmodules :: HS.HashSet T.Text -- ^ The set of ignored modules
    , _strictness     :: Strictness
    , _extraTests     :: Bool
    , _knownusers     :: [T.Text]
    , _knowngroups    :: [T.Text]
    }

data Defaults = Defaults
    { _defKnownusers  :: Maybe [T.Text]
    , _defKnowngroups :: Maybe [T.Text]
    }

makeClassy ''Preferences

instance FromJSON Defaults where
    parseJSON (Object v) = Defaults
                           <$> v .:? "knownusers"  .!= mempty
                           <*> v .:? "knowngroups"  .!= mempty
    parseJSON _ = error "Error parsing Facts"

genPreferences :: FilePath
               -> IO (Preferences IO)
genPreferences basedir = do
    let manifestdir = basedir <> "/manifests"
        modulesdir  = basedir <> "/modules"
        templatedir = basedir <> "/templates"
        testdir     = basedir <> "/tests"
    typenames <- fmap (map takeBaseName) (getFiles (T.pack modulesdir) "lib/puppet/type" ".rb")
    defaults <- loadDefaults (testdir ++ "/defaults.yaml")
    let loadedTypes = HM.fromList (map defaulttype typenames)
    return $ Preferences (PuppetDirPaths basedir manifestdir modulesdir templatedir testdir)
                         dummyPuppetDB (baseNativeTypes `HM.union` loadedTypes)
                         stdlibFunctions
                         (Just (basedir <> "/hiera.yaml")) mempty Strict True (getKnownusers defaults) (getKnowngroups defaults)

{-| Use lens with the set operator and composition to set external/custom params.
Ex.: @ setupPreferences workingDir ((hieraPath.~mypath) . (prefPDB.~pdbapi)) @
-}
setupPreferences :: FilePath -- ^ The base working directory
                 -> (Preferences IO -> Preferences IO) -- ^ Preference setting
                 -> IO (Preferences IO)
setupPreferences basedir k = fmap k (genPreferences basedir)

loadDefaults :: FilePath -> IO (Maybe Defaults)
loadDefaults fp = do
  p <- fileExist fp
  if p then loadYamlFile fp else return Nothing

getKnownusers :: Maybe Defaults -> [T.Text]
getKnownusers (Just def) = fromMaybe (getKnownusers Nothing) (_defKnownusers def)
getKnownusers Nothing = ["mysql", "vagrant","nginx", "nagios", "postgres", "puppet", "root", "syslog", "www-data"]

getKnowngroups :: Maybe Defaults -> [T.Text]
getKnowngroups (Just def) = fromMaybe (getKnowngroups Nothing) (_defKnowngroups def)
getKnowngroups Nothing = ["adm", "syslog", "mysql", "nagios","postgres", "puppet", "root", "www-data"]
