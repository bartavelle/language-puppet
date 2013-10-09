{-# LANGUAGE TemplateHaskell #-}
module PuppetDB.TestDB (initPuppetDB) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Either.Strict as S
import Puppet.Interpreter.Types
import Control.Lens
import Control.Concurrent.STM
import Data.Monoid

data PDBEntry = PDBEntry { _pdbfacts     :: Facts
                         , _lastCatalog  :: FinalCatalog
                         , _lastExported :: FinalCatalog
                         }

type PDBContent = Container PDBEntry

makeClassy ''PDBEntry

initPuppetDB :: IO (T.Text -> Value -> IO (S.Either String Value))
initPuppetDB = newTVarIO mempty >>= return . queryDB

queryDB :: TVar PDBContent -> T.Text -> Value -> IO (S.Either String Value)
queryDB _ r _ = return (S.Left ("Unknown endpoint: " ++ show r))
