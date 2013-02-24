module PuppetDB.Rest where

import qualified PuppetDB.Query as PDB

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import Data.Aeson
import qualified Codec.Text.IConv as IConv
import qualified Control.Exception as X
import Control.Monad.Error
import Puppet.Utils
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

pdbRequest :: (FromJSON a) => T.Text -> T.Text -> PDB.Query -> IO (Either String a)
pdbRequest url querytype qquery = rawRequest url querytype (PDB.showQuery qquery)

rawRequest :: (FromJSON a) => T.Text -> T.Text -> T.Text -> IO (Either String a)
rawRequest url querytype query = runErrorT $ do
        unless (querytype `elem` ["resources", "nodes", "facts"]) (throwError $ "Invalid query type " ++ T.unpack querytype)
        let q = case querytype of
                    "facts" -> T.cons '/' query
                    _       -> "?" <> (T.decodeUtf8 $ W.renderSimpleQuery False [("query", T.encodeUtf8 query)])
            fullurl = T.unpack $ url <> "/" <> querytype <> q
        initReq <- case (parseUrl fullurl :: Maybe (Request a)) of
            Just x -> return x
            Nothing -> throwError "Something failed when parsing the PuppetDB URL"
        let req = initReq { requestHeaders = [("Accept", "application/json")] }
        runRequest req
