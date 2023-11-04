{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Puppet.Runner.Preferences
  ( Preferences (Preferences),
    prefPuppetPaths,
    prefPDB,
    prefNatTypes,
    prefExtFuncs,
    prefHieraPath,
    prefIgnoredmodules,
    prefStrictness,
    prefExtraTests,
    prefKnownusers,
    prefKnowngroups,
    prefExternalmodules,
    prefPuppetSettings,
    prefFactsOverride,
    prefFactsDefault,
    prefLogLevel,
    prefRebaseFile,
    dfPreferences,
    PuppetDirPaths,
    HasPuppetDirPaths (..),
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Puppet.Interpreter
import qualified Puppet.Runner.Puppetlabs as Puppetlabs
import Puppet.Runner.Stdlib
import PuppetDB
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Log.Logger as LOG
import XPrelude

data Preferences m = Preferences
  { _prefPuppetPaths :: PuppetDirPaths,
    _prefPDB :: PuppetDBAPI m,
    -- | The list of native types.
    _prefNatTypes :: Container NativeTypeMethods,
    _prefExtFuncs :: Container ([PValue] -> InterpreterMonad PValue),
    _prefHieraPath :: Maybe FilePath,
    _prefIgnoredmodules :: HS.HashSet Text,
    _prefStrictness :: Strictness,
    _prefExtraTests :: Bool,
    _prefKnownusers :: [Text],
    _prefKnowngroups :: [Text],
    _prefExternalmodules :: HS.HashSet Text,
    -- | Puppet server settings
    _prefPuppetSettings :: Container Text,
    _prefFactsOverride :: Container PValue,
    _prefFactsDefault :: Container PValue,
    _prefLogLevel :: LOG.Priority,
    -- | Make all calls to file() with absolute pathes relative to the given path.
    _prefRebaseFile :: Maybe FilePath
  }

data Defaults = Defaults
  { _dfKnownusers :: Maybe [Text],
    _dfKnowngroups :: Maybe [Text],
    _dfIgnoredmodules :: Maybe [Text],
    _dfStrictness :: Maybe Strictness,
    _dfExtratests :: Maybe Bool,
    _dfExternalmodules :: Maybe [Text],
    _dfPuppetSettings :: Maybe (Container Text),
    _dfFactsDefault :: Maybe (Container PValue),
    _dfFactsOverride :: Maybe (Container PValue),
    _dfRebaseFile :: Maybe FilePath
  }
  deriving (Show)

makeLenses ''Preferences

instance FromJSON Defaults where
  parseJSON (Object v) =
    Defaults
      <$> v .:? "knownusers"
      <*> v .:? "knowngroups"
      <*> v .:? "ignoredmodules"
      <*> v .:? "strict"
      <*> v .:? "extratests"
      <*> v .:? "externalmodules"
      <*> v .:? "settings"
      <*> v .:? "factsdefault"
      <*> v .:? "factsoverride"
      <*> v .:? "rebasefile"
  parseJSON _ = mzero

-- | Generate default preferences.
dfPreferences ::
  FilePath ->
  IO (Preferences IO)
dfPreferences basedir = do
  let dirpaths = puppetPaths basedir
      modulesdir = dirpaths ^. modulesPath
      testdir = dirpaths ^. testPath
      hierafile = basedir <> "/hiera.yaml"
      defaultfile = testdir <> "/defaults.yaml"
  defaults <- ifM (Directory.doesFileExist defaultfile) (Yaml.decodeFileThrow defaultfile) (pure Nothing)
  hieradir <- ifM (Directory.doesFileExist hierafile) (pure $ Just hierafile) (pure Nothing)
  loadedtypes <- loadedTypes modulesdir
  labsFunctions <- Puppetlabs.extFunctions modulesdir
  return $
    Preferences
      dirpaths
      dummyPuppetDB
      (baseNativeTypes `HM.union` loadedtypes)
      (HM.union stdlibFunctions labsFunctions)
      hieradir
      (getIgnoredmodules defaults)
      (getStrictness defaults)
      (getExtraTests defaults)
      (getKnownusers defaults)
      (getKnowngroups defaults)
      (getExternalmodules defaults)
      (getPuppetSettings dirpaths defaults)
      (getFactsOverride defaults)
      (getFactsDefault defaults)
      LOG.NOTICE -- good default as INFO is quite noisy
      Nothing

loadedTypes :: FilePath -> IO (HM.HashMap NativeTypeName NativeTypeMethods)
loadedTypes modulesdir = do
  typenames <- map (Text.pack . FilePath.takeBaseName) <$> getFiles modulesdir "lib/puppet/type" ".rb"
  pure $ HM.fromList (map defaulttype typenames)
  where
    getFiles :: FilePath -> FilePath -> FilePath -> IO [FilePath]
    getFiles moduledir subdir extension =
      fmap concat $
        Directory.listDirectory moduledir
          >>= mapM (checkForSubFiles extension . (\x -> moduledir <> "/" <> x <> "/" <> subdir))
    checkForSubFiles :: FilePath -> FilePath -> IO [FilePath]
    checkForSubFiles extension dir =
      catch (fmap Right (Directory.listDirectory dir)) (\e -> return $ Left (e :: IOException)) >>= \case
        Right o -> return ((map (\x -> dir <> "/" <> x) . filter (List.isSuffixOf extension)) o)
        Left _ -> return []

-- Utilities for getting default values from the yaml file
-- It provides (the same) static defaults (see the 'Nothing' case) when
--     no default yaml file or
--     not key/value for the option has been provided
getKnownusers :: Maybe Defaults -> [Text]
getKnownusers = fromMaybe ["mysql", "vagrant", "nginx", "nagios", "postgres", "puppet", "root", "syslog", "www-data"] . (>>= _dfKnownusers)

getKnowngroups :: Maybe Defaults -> [Text]
getKnowngroups = fromMaybe ["adm", "syslog", "mysql", "nagios", "postgres", "puppet", "root", "www-data", "postfix"] . (>>= _dfKnowngroups)

getStrictness :: Maybe Defaults -> Strictness
getStrictness = fromMaybe Permissive . (>>= _dfStrictness)

getIgnoredmodules :: Maybe Defaults -> HS.HashSet Text
getIgnoredmodules = maybe mempty HS.fromList . (>>= _dfIgnoredmodules)

getExtraTests :: Maybe Defaults -> Bool
getExtraTests = fromMaybe True . (>>= _dfExtratests)

getExternalmodules :: Maybe Defaults -> HS.HashSet Text
getExternalmodules = maybe mempty HS.fromList . (>>= _dfExternalmodules)

getPuppetSettings :: PuppetDirPaths -> Maybe Defaults -> Container Text
getPuppetSettings dirpaths = fromMaybe df . (>>= _dfPuppetSettings)
  where
    df :: Container Text
    df =
      HM.fromList
        [ ("confdir", Text.pack $ dirpaths ^. baseDir),
          ("strict_variables", "true")
        ]

getFactsOverride :: Maybe Defaults -> Container PValue
getFactsOverride = fromMaybe mempty . (>>= _dfFactsOverride)

getFactsDefault :: Maybe Defaults -> Container PValue
getFactsDefault = fromMaybe mempty . (>>= _dfFactsDefault)
