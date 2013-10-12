{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.TestDB (initPuppetDB) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import qualified Data.Vector as V
import Control.Lens
import Control.Concurrent.STM
import Data.Monoid
import Control.Applicative

import Puppet.Interpreter.Types
import PuppetDB.Types

initPuppetDB :: IO PuppetDBAPI
initPuppetDB = undefined
