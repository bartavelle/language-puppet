module PuppetDB.Common where

import Puppet.PP
import Puppet.Interpreter.Types
import PuppetDB.Remote
import PuppetDB.Dummy
import PuppetDB.TestDB
import Data.Maybe
import Data.List (stripPrefix)

import System.Environment
import qualified Data.Either.Strict as S

data PDBType = PDBRemote | PDBDummy | PDBTest

instance Read PDBType where
    readsPrec _ r | isJust reml = [(PDBRemote, fromJust reml)]
                  | isJust rems = [(PDBRemote, fromJust rems)]
                  | isJust duml = [(PDBDummy, fromJust duml)]
                  | isJust dums = [(PDBDummy, fromJust dums)]
                  | isJust tstl = [(PDBTest, fromJust tstl)]
                  | isJust tsts = [(PDBTest, fromJust tsts)]
                  | otherwise   = []
        where
            reml = stripPrefix "PDBRemote" r
            rems = stripPrefix "remote"    r
            duml = stripPrefix "PDBDummy"  r
            dums = stripPrefix "dummy"     r
            tstl = stripPrefix "PDBTest"   r
            tsts = stripPrefix "test"      r

getDefaultDB :: PDBType -> IO (S.Either Doc PuppetDBAPI)
getDefaultDB PDBDummy  = return (S.Right dummyPuppetDB)
getDefaultDB PDBRemote = pdbConnect "http://localhost:8080"
getDefaultDB PDBTest   = lookupEnv "HOME" >>= \case
                                Just h -> loadTestDB (h ++ "/.testdb")
                                Nothing -> fmap S.Right initTestDB

generateWireCatalog :: FinalCatalog -> EdgeMap -> WireCatalog
generateWireCatalog = undefined
