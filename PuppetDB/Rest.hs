{-# LANGUAGE OverloadedStrings #-}

module PuppetDB.Rest where

import Puppet.Interpreter.Types
import qualified PuppetDB.Query as PDB

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Attoparsec.Number
import qualified Codec.Text.IConv as IConv
import qualified Control.Exception as X
import Control.Monad.Error

instance FromJSON ResolvedValue where
    parseJSON Null = return ResolvedUndefined
    parseJSON (Number x) = return $ case x of
                                        (I n) -> ResolvedInt n
                                        (D d) -> ResolvedDouble d
    parseJSON (String s) = return $ ResolvedString $ T.unpack s
    parseJSON (Array a) = fmap ResolvedArray (mapM parseJSON (V.toList a))
    parseJSON (Object o) = fmap ResolvedHash (mapM (\(a,b) -> do {
                                                                 b' <- parseJSON b ;
                                                                 return (T.unpack a,b') }
                                                                 ) (HM.toList o))
    parseJSON (Bool b) = return $ ResolvedBool b

toAndQuery = undefined

simpleNodeQuery :: String -> [(String, String)] -> IO (Either String ResolvedValue)
simpleNodeQuery url query = rawRequest url "nodes" (toAndQuery query)

simpleResourceQuery :: String -> [(String, String)] -> IO (Either String ResolvedValue)
simpleResourceQuery url query = rawRequest url "resources" (toAndQuery query)

runRequest req = do
    let doRequest = withManager (\manager -> fmap responseBody $ httpLbs req manager) :: IO L.ByteString
        eHandler :: X.SomeException -> IO (Either String  L.ByteString)
        eHandler e = return $ Left $ show e
    mo <- liftIO ((fmap Right doRequest) `X.catch` eHandler)
    case mo of
        Right o -> do
            let utf8 = IConv.convert "LATIN1" "UTF-8" o
            case decode' utf8 :: Maybe ResolvedValue of
                Just x                   -> return x
                Nothing                  -> throwError "Json decoding has failed"
        Left err -> throwError err

pdbRequest :: String -> String -> PDB.Query -> IO (Either String ResolvedValue)
pdbRequest url querytype qquery = rawRequest url querytype (PDB.showQuery qquery)

rawRequest :: String -> String -> String -> IO (Either String ResolvedValue)
rawRequest url querytype query = runErrorT $ do
        unless (querytype `elem` ["resources", "nodes", "facts"]) (throwError $ "Invalid query type " ++ querytype)
        let q = case querytype of
                    "facts" -> '/' : query
                    _       -> "?" ++ (BC.unpack $ W.renderSimpleQuery False [("query", BC.pack query)])
        initReq <- case (parseUrl (url ++ "/" ++ querytype ++ q) :: Maybe (Request a)) of
            Just x -> return x
            Nothing -> throwError "Something failed when parsing the PuppetDB URL"
        let req = initReq { requestHeaders = [("Accept", "application/json")] }
        runRequest req
