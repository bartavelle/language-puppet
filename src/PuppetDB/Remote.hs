{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module PuppetDB.Remote (pdbConnect) where

import           XPrelude

import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client

import           Facter
import           Puppet.Language
import           PuppetDB.Core


type PDBAPIv3 =    "nodes"     :> QueryParam "query" (Query NodeField)       :> Get '[JSON] [NodeInfo]
              :<|> "nodes"     :> Capture "resourcename" Text :> "resources" :> QueryParam "query" (Query ResourceField) :> Get '[JSON] [Resource]
              :<|> "facts"     :> QueryParam "query" (Query FactField)       :> Get '[JSON] [FactInfo]
              :<|> "resources" :> QueryParam "query" (Query ResourceField)   :> Get '[JSON] [Resource]

type PDBAPI = "v3" :> PDBAPIv3

api :: Proxy PDBAPI
api = Proxy

-- | Given an URL (ie. @http://localhost:8080@), will return an incomplete 'PuppetDBAPI'.
pdbConnect :: Manager -> String -> IO (Either PrettyError (PuppetDBAPI IO))
pdbConnect mgr url = do
  url' <- parseBaseUrl url
  let env = ClientEnv mgr url'
  pure $ Right $ PuppetDBAPI
    (return (ppline $ fromString url))
    (const (throwError "operation not supported"))
    (const (throwError "operation not supported"))
    (const (throwError "operation not supported"))
    (\q -> prettyError $ runClientM (sgetFacts (Just q)) env)
    (\q -> prettyError $ runClientM (sgetResources (Just q)) env)
    (\q -> prettyError $ runClientM (sgetNodes (Just q)) env)
    (throwError "operation not supported")
    (\node q -> prettyError $ runClientM (sgetNodeResources node (Just q)) env)
  where
   sgetNodes :: Maybe (Query NodeField) -> ClientM [NodeInfo]
   sgetNodeResources :: Text -> Maybe (Query ResourceField) -> ClientM [Resource]
   sgetFacts :: Maybe (Query FactField) -> ClientM [FactInfo]
   sgetResources :: Maybe (Query ResourceField) -> ClientM [Resource]
   (sgetNodes :<|> sgetNodeResources :<|> sgetFacts :<|> sgetResources) = client api

   prettyError :: IO (Either ServantError b)  -> ExceptT PrettyError IO b
   prettyError = ExceptT . fmap (_Left %~ PrettyError . pplines . show)
