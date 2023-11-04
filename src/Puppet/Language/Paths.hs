{-# LANGUAGE TemplateHaskell #-}

module Puppet.Language.Paths where

import XPrelude

data PuppetDirPaths = PuppetDirPaths
  { -- | Puppet base working directory
    _baseDir :: FilePath,
    -- | The path to the manifests.
    _manifestPath :: FilePath,
    -- | The path to the modules.
    _modulesPath :: FilePath,
    -- | The path to the template.
    _templatesPath :: FilePath,
    -- | The path to a tests folders to hold tests files such as the pdbfiles.
    _testPath :: FilePath
  }

makeClassy ''PuppetDirPaths

puppetPaths :: FilePath -> PuppetDirPaths
puppetPaths basedir = PuppetDirPaths basedir manifestdir modulesdir templatedir testdir
  where
    manifestdir = basedir <> "/manifests"
    modulesdir = basedir <> "/modules"
    templatedir = basedir <> "/templates"
    testdir = basedir <> "/tests"
