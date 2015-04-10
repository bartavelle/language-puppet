{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Puppet.Preferences (
    dfPreferences
  , HasPreferences(..)
  , Preferences(Preferences)
  , PuppetDirPaths
  , HasPuppetDirPaths(..)
) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
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
    { _puppetPaths     :: PuppetDirPaths
    , _prefPDB         :: PuppetDBAPI m
    , _natTypes        :: Container NativeTypeMethods -- ^ The list of native types.
    , _prefExtFuncs    :: Container ( [PValue] -> InterpreterMonad PValue )
    , _hieraPath       :: Maybe FilePath
    , _ignoredmodules  :: HS.HashSet Text
    , _strictness      :: Strictness
    , _extraTests      :: Bool
    , _knownusers      :: [Text]
    , _knowngroups     :: [Text]
    , _externalmodules :: HS.HashSet Text
    }

data Defaults = Defaults
    { _dfKnownusers      :: Maybe [Text]
    , _dfKnowngroups     :: Maybe [Text]
    , _dfIgnoredmodules  :: Maybe [Text]
    , _dfStrictness      :: Maybe Strictness
    , _dfExtratests      :: Maybe Bool
    , _dfExternalmodules :: Maybe [Text]
    } deriving Show


makeClassy ''Preferences

instance FromJSON Defaults where
    parseJSON (Object v) = Defaults
                           <$> v .:? "knownusers"
                           <*> v .:? "knowngroups"
                           <*> v .:? "ignoredmodules"
                           <*> v .:? "strict"
                           <*> v .:? "extratests"
                           <*> v .:? "externalmodules"
    parseJSON _ = mzero

-- | generate default preferences
dfPreferences :: FilePath
               -> IO (Preferences IO)
dfPreferences basedir = do
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
                         (Just (basedir <> "/hiera.yaml"))
                         (getIgnoredmodules defaults)
                         (getStrictness defaults)
                         (getExtraTests defaults)
                         (getKnownusers defaults)
                         (getKnowngroups defaults)
                         (getExternalmodules defaults)


loadDefaults :: FilePath -> IO (Maybe Defaults)
loadDefaults fp = do
  p <- fileExist fp
  if p then loadYamlFile fp else return Nothing

-- Utilities for getting default values from the yaml file
-- It provides (the same) static defaults (see the 'Nothing' case) when
--     no default yaml file or
--     not key/value for the option has been provided
getKnownusers :: Maybe Defaults -> [Text]
getKnownusers (Just df) = fromMaybe (getKnownusers Nothing) (_dfKnownusers df)
getKnownusers Nothing = ["mysql", "vagrant","nginx", "nagios", "postgres", "puppet", "root", "syslog", "www-data"]

getKnowngroups :: Maybe Defaults -> [Text]
getKnowngroups (Just df) = fromMaybe (getKnowngroups Nothing) (_dfKnowngroups df)
getKnowngroups Nothing = ["adm", "syslog", "mysql", "nagios","postgres", "puppet", "root", "www-data"]

getStrictness :: Maybe Defaults -> Strictness
getStrictness (Just df) = fromMaybe (getStrictness Nothing) (_dfStrictness df)
getStrictness Nothing = Permissive

getIgnoredmodules :: Maybe Defaults -> HS.HashSet Text
getIgnoredmodules (Just df) = maybe (getIgnoredmodules Nothing) HS.fromList (_dfIgnoredmodules df)
getIgnoredmodules Nothing = mempty

getExtraTests :: Maybe Defaults -> Bool
getExtraTests (Just df) = fromMaybe (getExtraTests Nothing) (_dfExtratests df)
getExtraTests Nothing = True

getExternalmodules :: Maybe Defaults -> HS.HashSet Text
getExternalmodules (Just df) = maybe (getExternalmodules Nothing) HS.fromList (_dfExternalmodules df)
getExternalmodules Nothing = mempty
