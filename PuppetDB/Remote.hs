{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module PuppetDB.Remote (pdbConnect) where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Proxy
import           Data.Text                (Text)
import           Network.HTTP.Client      (Manager)
import           Servant.API
import           Servant.Client

import           Puppet.Interpreter.Types
import           Puppet.PP

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
        (return (string $ show url))
        (const (throwError "operation not supported"))
        (const (throwError "operation not supported"))
        (const (throwError "operation not supported"))
        (q1 sgetFacts)
        (q1 sgetResources)
        (q1 sgetNodes)
        (throwError "operation not supported")
        (\ndename q -> prettyError $ sgetNodeResources ndename (Just q) mgr url)
        where
            sgetNodes :: Maybe (Query NodeField) -> Manager -> BaseUrl -> ClientM [NodeInfo]
            sgetNodeResources :: Text -> Maybe (Query ResourceField) -> Manager -> BaseUrl -> ClientM [Resource]
            sgetFacts :: Maybe (Query FactField) -> Manager -> BaseUrl -> ClientM [FactInfo]
            sgetResources :: Maybe (Query ResourceField) -> Manager -> BaseUrl -> ClientM [Resource]
            (sgetNodes :<|> sgetNodeResources :<|> sgetFacts :<|> sgetResources) = client api

            prettyError :: ExceptT ServantError IO b -> ExceptT PrettyError IO b
            prettyError = mapExceptT (fmap (_Left %~ PrettyError . string. show))
            q1 :: FromJSON b => (Maybe a -> Manager -> BaseUrl -> ClientM b) -> a -> ExceptT PrettyError IO b
            q1 f a = prettyError (f (Just a) mgr url)
