{-# LANGUAGE LambdaCase #-}
-- | Common data types for PuppetDB.
module PuppetDB.Common where

import Puppet.Interpreter.Types
import PuppetDB.Remote
import PuppetDB.Dummy
import PuppetDB.TestDB

import Data.Maybe
import Data.List (stripPrefix)
import Control.Lens
import System.Environment
import Data.Vector.Lens
import Servant.Common.BaseUrl

-- | The supported PuppetDB implementations.
data PDBType = PDBRemote -- ^ Your standard PuppetDB, queried through the HTTP interface.
             | PDBDummy -- ^ A stupid stub, this is the default choice.
             | PDBTest -- ^ A slow but handy PuppetDB implementation that is backed by a YAML file.
             deriving Eq

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

-- | Given a 'PDBType', will try return a sane default implementation.
getDefaultDB :: PDBType -> IO (Either PrettyError (PuppetDBAPI IO))
getDefaultDB PDBDummy  = return (Right dummyPuppetDB)
getDefaultDB PDBRemote = let Right url = parseBaseUrl "http://localhost:8080"
                         in  pdbConnect url
getDefaultDB PDBTest   = lookupEnv "HOME" >>= \case
                                Just h -> loadTestDB (h ++ "/.testdb")
                                Nothing -> fmap Right initTestDB

-- | Turns a 'FinalCatalog' and 'EdgeMap' into a document that can be
-- serialized and fed to @puppet apply@.
generateWireCatalog :: Nodename -> FinalCatalog -> EdgeMap -> WireCatalog
generateWireCatalog ndename finalcat edgemap = WireCatalog ndename "version" edges resources "uiid"
    where
        edges     = toVectorOf (folded . to (\li -> PuppetEdge (li ^. linksrc) (li ^. linkdst) (li ^. linkType))) (concatOf folded edgemap)
        resources = toVectorOf folded finalcat
