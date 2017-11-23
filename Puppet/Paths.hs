{-# LANGUAGE TemplateHaskell #-}
module Puppet.Paths where

import Puppet.Prelude

data PuppetDirPaths = PuppetDirPaths
    { _baseDir       :: FilePath -- ^ Puppet base working directory
    , _manifestPath  :: FilePath -- ^ The path to the manifests.
    , _modulesPath   :: FilePath -- ^ The path to the modules.
    , _templatesPath :: FilePath -- ^ The path to the template.
    , _testPath      :: FilePath -- ^ The path to a tests folders to hold tests files such as the pdbfiles.
    }

makeClassy ''PuppetDirPaths

puppetPaths :: FilePath -> PuppetDirPaths
puppetPaths basedir = PuppetDirPaths basedir manifestdir modulesdir templatedir testdir
    where
        manifestdir = basedir <> "/manifests"
        modulesdir  = basedir <> "/modules"
        templatedir = basedir <> "/templates"
        testdir     = basedir <> "/tests"
