-- | Common data types for PuppetDB.
module PuppetDB
  ( dummyPuppetDB
  , getDefaultDB
  , pdbConnect
  , loadTestDB
  , generateWireCatalog
  , puppetDBFacts
  , module PuppetDB.Core
  ) where

import           XPrelude

import qualified Data.HashMap.Strict    as Map
import           Control.Arrow ((***))
import qualified Data.Text              as Text
import           Data.Vector.Lens
import           Network.HTTP.Client
import           System.Environment

import           Facter
import           Puppet.Language
import           PuppetDB.Core
import           PuppetDB.Remote
import           PuppetDB.TestDB



-- | Given a 'PDBType', will try return a sane default implementation.
getDefaultDB :: PDBType -> IO (Either PrettyError (PuppetDBAPI IO))
getDefaultDB PDBDummy  = return (Right dummyPuppetDB)
getDefaultDB PDBRemote = do
  let url = "http://localhost:8080"
  mgr <- newManager defaultManagerSettings
  pdbConnect mgr url
getDefaultDB PDBTest =
  lookupEnv "HOME" >>= \case
    Just h -> loadTestDB (h <> "/.testdb")
    Nothing -> fmap Right initTestDB


-- | A dummy implementation of 'PuppetDBAPI', that will return empty responses.
dummyPuppetDB :: Monad m => PuppetDBAPI m
dummyPuppetDB =
  PuppetDBAPI
    (return "dummy")
    (const (return ()))
    (const (return ()))
    (const (return ()))
    (const (throwError "not implemented"))
    (const (return []))
    (const (return []))
    (throwError "not implemented")
    (\_ _ -> return [])

-- | Turns a 'FinalCatalog' and 'EdgeMap' into a document that can be
-- serialized and fed to @puppet apply@.
generateWireCatalog :: NodeName -> FinalCatalog -> EdgeMap -> WireCatalog
generateWireCatalog node cat edgemap = WireCatalog node "version" edges resources "uiid"
  where
    edges     = toVectorOf (folded . to (\li -> PuppetEdge (li ^. linksrc) (li ^. linkdst) (li ^. linkType))) (concatOf folded edgemap)
    resources = toVectorOf folded cat

puppetDBFacts :: NodeName -> PuppetDBAPI IO -> IO (HashMap Text PValue)
puppetDBFacts node pdbapi =
  runExceptT (getPDBFacts pdbapi (QEqual FCertname node)) >>= \case
    Right facts@(_:_) -> return (Map.fromList (map (\f -> (f ^. factInfoName, f ^. factInfoVal)) facts))
    _ -> do
        rawFacts <- fmap concat (sequence [factNET, factRAM, factOS, fversion, factMountPoints, factOS, factUser, factUName, fenv, factProcessor])
        let ofacts = genFacts $ map (Text.pack *** Text.pack) rawFacts
            (hostname, ddomainname) = Text.break (== '.') node
            domainname = if Text.null ddomainname
                           then ""
                           else Text.tail ddomainname
            nfacts = genFacts [ ("fqdn", node)
                              , ("hostname", hostname)
                              , ("domain", domainname)
                              , ("rootrsa", "xxx")
                              , ("operatingsystem", "Ubuntu")
                              , ("puppetversion", "language-puppet")
                              , ("virtual", "xenu")
                              , ("clientcert", node)
                              , ("is_virtual", "true")
                              , ("concat_basedir", "/var/lib/puppet/concat")
                              ]
            allfacts = nfacts `Map.union` ofacts
            genFacts = Map.fromList
        return (allfacts & traverse %~ PString & buildOSHash)

buildOSHash :: Facts -> Facts
buildOSHash facts = case buildObject topLevel of
                      Nothing -> facts
                      Just os -> facts & at "os" ?~ os
  where
    buildObject keys =
      let nobject = foldl' addKey mempty keys
      in  if nobject == mempty
            then Nothing
            else Just (PHash nobject)
    g k = facts ^? ix k
    topLevel = [ ("name", g "operatingsystem")
               , ("family", g "osfamily")
               , ("release", buildObject [("major", g "lsbdistrelease"), ("full", g "lsbdistrelease")])
               , ("lsb", buildObject [ ("distcodename", g "lsbdistcodename")
                                     , ("distid", g "lsbdistid")
                                     , ("distdescription", g "lsbdistdescription")
                                     , ("distrelease", g "lsbdistrelease")
                                     , ("majdistrelease", g "lsbmajdistrelease")
                                     ])
               ]
    addKey hash (k, mv) = case mv of
                           Nothing -> hash
                           Just v -> hash & at k ?~ v
