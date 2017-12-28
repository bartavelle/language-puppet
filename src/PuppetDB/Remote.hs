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
pdbConnect :: Manager -> BaseUrl -> IO (Either PrettyError (PuppetDBAPI IO))
pdbConnect mgr url =
  return $ Right $ PuppetDBAPI
    (return (ppline $ show url))
    (const (throwError "operation not supported"))
    (const (throwError "operation not supported"))
    (const (throwError "operation not supported"))
    (q1 sgetFacts)
    (q1 sgetResources)
    (q1 sgetNodes)
    (throwError "operation not supported")
    (\ndename q -> prettyError $ sgetNodeResources ndename (Just q))
    where
     sgetNodes :: Maybe (Query NodeField) -> ClientM [NodeInfo]
     sgetNodeResources :: Text -> Maybe (Query ResourceField) -> ClientM [Resource]
     sgetFacts :: Maybe (Query FactField) -> ClientM [FactInfo]
     sgetResources :: Maybe (Query ResourceField) -> ClientM [Resource]
     (sgetNodes :<|> sgetNodeResources :<|> sgetFacts :<|> sgetResources) = client api

     prettyError :: ClientM b -> ExceptT PrettyError IO b
     prettyError = ExceptT . fmap (_Left %~ PrettyError . pplines . show) . flip runClientM (ClientEnv mgr url)
     q1 :: (Maybe a -> ClientM b) -> a -> ExceptT PrettyError IO b
     q1 f a = prettyError (f (Just a))
