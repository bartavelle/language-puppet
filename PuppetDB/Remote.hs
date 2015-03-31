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
import qualified Data.Either.Strict as S
import Servant.API
import Servant.Client
import Servant.Common.Text
import Data.Aeson
import Data.Proxy

type PDBAPIv3 =    "nodes"     :> QueryParam "query" (Query NodeField)     :> Get [PNodeInfo]
              :<|> "nodes"     :> Capture "resourcename" Text :> "resources" :> QueryParam "query" (Query ResourceField) :> Get [Resource]
              :<|> "facts"     :> QueryParam "query" (Query FactField)     :> Get [PFactInfo]
              :<|> "resources" :> QueryParam "query" (Query ResourceField) :> Get [Resource]

type PDBAPI = "v3" :> PDBAPIv3

spdbAPI :: Proxy PDBAPI
spdbAPI = Proxy

sgetNodes :: Maybe (Query NodeField) -> BaseUrl -> EitherT String IO [PNodeInfo]
sgetNodeResources :: Text -> Maybe (Query ResourceField) -> BaseUrl -> EitherT String IO [Resource]
sgetFacts :: Maybe (Query FactField) -> BaseUrl -> EitherT String IO [PFactInfo]
sgetResources :: Maybe (Query ResourceField) -> BaseUrl -> EitherT String IO [Resource]
(     sgetNodes
 :<|> sgetNodeResources
 :<|> sgetFacts
 :<|> sgetResources
 ) = client spdbAPI

-- | Given an URL (ie. @http://localhost:8080@), will return an incomplete 'PuppetDBAPI'.
pdbConnect :: BaseUrl -> IO (S.Either PrettyError (PuppetDBAPI IO))
pdbConnect url = return $ S.Right $ PuppetDBAPI
    (return (string $ show url))
    (const (left "operation not supported"))
    (const (left "operation not supported"))
    (const (left "operation not supported"))
    (q1 sgetFacts)
    (q1 sgetResources)
    (q1 sgetNodes)
    (left "operation not supported")
    (\ndename q -> prettyError $ sgetNodeResources ndename (Just q) url)
    where
        prettyError :: EitherT String IO b -> EitherT PrettyError IO b
        prettyError = bimapEitherT (PrettyError . string) id
        q1 :: (ToText a, FromJSON b) => (Maybe a -> BaseUrl -> EitherT String IO b) -> a -> EitherT PrettyError IO b
        q1 f x = prettyError $ f (Just x) url

