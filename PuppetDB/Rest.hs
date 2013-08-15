module PuppetDB.Rest where

import Puppet.Utils

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BC
import Data.Aeson
import qualified Codec.Text.IConv as IConv
import qualified Control.Exception as X
import Control.Monad.Error
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Either.Strict as S

runRequest req = do
    let doRequest = withManager (fmap responseBody . httpLbs req) :: IO L.ByteString
        eHandler :: X.SomeException -> IO (Either String  L.ByteString)
        eHandler e = return $ Left $ show e ++ ", with queryString " ++ BC.unpack (queryString req)
    mo <- liftIO (fmap Right doRequest `X.catch` eHandler)
    case mo of
        Right o -> do
            let utf8 = IConv.convert "LATIN1" "UTF-8" o
            case decode' utf8 of
                Just x                   -> return x
                Nothing                  -> throwError "Json decoding has failed"
        Left err -> throwError err

pdbRequest :: (FromJSON a, ToJSON b) => T.Text -> T.Text -> b -> IO (S.Either String a)
pdbRequest url querytype query = fmap strictifyEither $ runErrorT $ do
    unless (querytype `elem` ["resources", "nodes", "facts"]) (throwError $ "Invalid query type " ++ T.unpack querytype)
    let jsonquery = L.toStrict (encode query)
        q = T.decodeUtf8 $ case querytype of
                "facts" -> BC.cons '/' jsonquery
                _       -> "?" <> W.renderSimpleQuery False [("query", jsonquery)]
        fullurl = url <> "/v1/" <> querytype <> q
    initReq <- case (parseUrl (T.unpack fullurl) :: Maybe (Request a)) of
        Just x -> return x
        Nothing -> throwError "Something failed when parsing the PuppetDB URL"
    let req = initReq { requestHeaders = [("Accept", "application/json")] }
    runRequest req

