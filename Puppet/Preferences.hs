module Puppet.Preferences where

import System.IO

data Prefs = Prefs {
    manifest :: FilePath,
    modules :: FilePath,
    templates :: FilePath,
    compilepoolsize :: Int,
    parsepoolsize :: Int
} deriving (Show)

genPrefs :: String -> Prefs
genPrefs basedir = Prefs (basedir ++ "/manifests") (basedir ++ "/modules") (basedir ++ "/templates") 1 1

