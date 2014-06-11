{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Puppet.Preferences where

import Puppet.Utils
import Puppet.Interpreter.Types
import Puppet.Plugins
import Puppet.NativeTypes
import Puppet.NativeTypes.Helpers
import Puppet.Stdlib
import PuppetDB.Dummy

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Lens

data Preferences m = Preferences
    { _manifestPath    :: FilePath -- ^ The path to the manifests.
    , _modulesPath     :: FilePath -- ^ The path to the modules.
    , _templatesPath   :: FilePath -- ^ The path to the template.
    , _prefPDB         :: PuppetDBAPI m
    , _natTypes        :: Container PuppetTypeMethods -- ^ The list of native types.
    , _prefExtFuncs    :: Container ( [PValue] -> InterpreterMonad PValue )
    , _hieraPath       :: Maybe FilePath
    , _ignoredmodules  :: HS.HashSet T.Text -- ^ The set of ignored modules
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
    return $ Preferences manifestdir modulesdir templatedir dummyPuppetDB (baseNativeTypes `HM.union` loadedTypes) (stdlibFunctions) (Just (basedir <> "/hiera.yaml")) mempty
