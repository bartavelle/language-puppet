{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Puppet.Preferences (
    setupPreferences
  , HasPreferences(..)
  , Preferences(Preferences)
  , HasPuppetDirPaths(..)
) where

import           Puppet.Interpreter.Types
import           Puppet.NativeTypes
import           Puppet.NativeTypes.Helpers
import           Puppet.Plugins
import           Puppet.Stdlib
import           Puppet.Utils
import           PuppetDB.Dummy

import           Control.Lens
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.Text                  as T

data PuppetDirPaths = PuppetDirPaths
    { _baseDir       :: FilePath -- ^ Puppet base working directory
    , _manifestPath  :: FilePath -- ^ The path to the manifests.
    , _modulesPath   :: FilePath -- ^ The path to the modules.
    , _templatesPath :: FilePath -- ^ The path to the template.
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
    }

makeClassy ''Preferences

genPreferences :: FilePath
               -> IO (Preferences IO)
genPreferences basedir = do
    let manifestdir = basedir <> "/manifests"
        modulesdir  = basedir <> "/modules"
        templatedir = basedir <> "/templates"
    typenames <- fmap (map takeBaseName) (getFiles (T.pack modulesdir) "lib/puppet/type" ".rb")
    let loadedTypes = HM.fromList (map defaulttype typenames)
    return $ Preferences (PuppetDirPaths basedir manifestdir modulesdir templatedir) dummyPuppetDB (baseNativeTypes `HM.union` loadedTypes) (stdlibFunctions) (Just (basedir <> "/hiera.yaml")) mempty Strict True

{-| Use lens with the set operator and composition to set external/custom params.

Ex.: @ setupPreferences workingDir ((hieraPath.~mypath) . (prefPDB.~pdbapi)) @
-}
setupPreferences :: FilePath -- ^ The base working directory
                 -> (Preferences IO -> Preferences IO) -- ^ Preference setting
                 -> IO (Preferences IO)
setupPreferences basedir k =
  fmap k (genPreferences basedir)
