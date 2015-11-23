{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module PuppetDB.Remote (pdbConnect) where

import Puppet.PP

import Puppet.Interpreter.Types
import Data.Text (Text)
import Control.Monad.Trans.Either
import Servant.API
import Servant.Client
import Data.Aeson
import Data.Proxy

type PDBAPIv3 =    "nodes"     :> QueryParam "query" (Query NodeField)       :> Get '[JSON] [NodeInfo]
              :<|> "nodes"     :> Capture "resourcename" Text :> "resources" :> QueryParam "query" (Query ResourceField) :> Get '[JSON] [Resource]
              :<|> "facts"     :> QueryParam "query" (Query FactField)       :> Get '[JSON] [FactInfo]
              :<|> "resources" :> QueryParam "query" (Query ResourceField)   :> Get '[JSON] [Resource]

type PDBAPI = "v3" :> PDBAPIv3

api :: Proxy PDBAPI
api = Proxy

-- | Given an URL (ie. @http://localhost:8080@), will return an incomplete 'PuppetDBAPI'.
pdbConnect :: BaseUrl -> IO (Either PrettyError (PuppetDBAPI IO))
pdbConnect url =
    return $ Right $ PuppetDBAPI
        (return (string $ show url))
        (const (left "operation not supported"))
        (const (left "operation not supported"))
        (const (left "operation not supported"))
        (q1 sgetFacts)
        (q1 sgetResources)
        (q1 sgetNodes)
        (left "operation not supported")
        (\ndename q -> prettyError $ sgetNodeResources ndename (Just q))
        where
            sgetNodes :: Maybe (Query NodeField) -> EitherT ServantError IO [NodeInfo]
            sgetNodeResources :: Text -> Maybe (Query ResourceField) -> EitherT ServantError IO [Resource]
            sgetFacts :: Maybe (Query FactField) -> EitherT ServantError IO [FactInfo]
            sgetResources :: Maybe (Query ResourceField) -> EitherT ServantError IO [Resource]
            (sgetNodes :<|> sgetNodeResources :<|> sgetFacts :<|> sgetResources) = client api url

            prettyError :: EitherT ServantError IO b -> EitherT PrettyError IO b
            prettyError = bimapEitherT (PrettyError . string. show) id
            q1 :: (ToText a, FromJSON b) => (Maybe a -> EitherT ServantError IO b) -> a -> EitherT PrettyError IO b
            q1 f = prettyError . f . Just
