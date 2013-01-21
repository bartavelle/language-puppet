{-# LANGUAGE OverloadedStrings #-}

module PuppetDB.Rest where

import Puppet.Interpreter.Types
import qualified PuppetDB.Query as PDB

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import Data.Aeson
import qualified Codec.Text.IConv as IConv
import qualified Control.Exception as X
import Control.Monad.Error
import qualified Data.Map as Map

runRequest req = do
    let doRequest = withManager (\manager -> fmap responseBody $ httpLbs req manager) :: IO L.ByteString
        eHandler :: X.SomeException -> IO (Either String  L.ByteString)
        eHandler e = return $ Left $ show e ++ ", with queryString " ++ (BC.unpack $ queryString req)
    mo <- liftIO ((fmap Right doRequest) `X.catch` eHandler)
    case mo of
        Right o -> do
            let utf8 = IConv.convert "LATIN1" "UTF-8" o
            case decode' utf8 of
                Just x                   -> return x
                Nothing                  -> throwError "Json decoding has failed"
        Left err -> throwError err

isNotLocal :: String -> CResource -> Bool
isNotLocal fqdn cr = case Map.lookup (Right "EXPORTEDSOURCE") (crparams cr) of
                        Just (Right (ResolvedString x)) -> x /= fqdn
                        _ -> True

pdbResRequest :: String -> String -> PDB.Query -> IO (Either String [CResource])
pdbResRequest url fqdn qquery = do
        res <- rawRequest url "resources" (PDB.showQuery qquery)
        case res of
            Left x -> return $ Left x
            Right y -> return $ Right $ filter ( isNotLocal fqdn ) y

pdbRequest :: String -> String -> PDB.Query -> IO (Either String ResolvedValue)
pdbRequest url querytype qquery = rawRequest url querytype (PDB.showQuery qquery)

rawRequest :: (FromJSON a) => String -> String -> String -> IO (Either String a)
rawRequest url querytype query = runErrorT $ do
        unless (querytype `elem` ["resources", "nodes", "facts"]) (throwError $ "Invalid query type " ++ querytype)
        let q = case querytype of
                    "facts" -> '/' : query
                    _       -> "?" ++ (BC.unpack $ W.renderSimpleQuery False [("query", BC.pack query)])
            fullurl = url ++ "/" ++ querytype ++ q
        initReq <- case (parseUrl fullurl :: Maybe (Request a)) of
            Just x -> return x
            Nothing -> throwError "Something failed when parsing the PuppetDB URL"
        let req = initReq { requestHeaders = [("Accept", "application/json")] }
        runRequest req
