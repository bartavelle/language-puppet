module Puppet.Preferences where

import System.IO

data Prefs = Prefs {
    manifest :: FilePath,
    modules :: FilePath,
    templates :: FilePath
} deriving (Show)
