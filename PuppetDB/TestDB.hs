{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.TestDB (loadTestDB,initTestDB) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.Vector as V
import Control.Lens
import Control.Concurrent.STM
import Data.Monoid
import Control.Applicative

import Puppet.Interpreter.Types
import Puppet.PP

loadTestDB :: FilePath -> IO (S.Either Doc PuppetDBAPI)
loadTestDB _ = undefined

initTestDB :: IO PuppetDBAPI
initTestDB = undefined
