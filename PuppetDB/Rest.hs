{-# LANGUAGE OverloadedStrings #-}

module PuppetDB.Rest where

import qualified Puppet.DSL.Types as DT
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
import Control.Applicative
import qualified Text.Parsec.Pos as TPP

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

instance FromJSON CResource where
    parseJSON (Object o) = do
        utitle     <- o .: "title"
        params     <- o .: "parameters"
        sourcefile <- o .: "sourcefile"
        sourceline <- o .: "sourceline"
        certname   <- o .: "certname"
        let _ = params :: HM.HashMap String ResolvedValue
            parameters = map (\(k,v) -> (Right k, Right v)) $ ("EXPORTEDSOURCE", ResolvedString certname) : HM.toList params :: [(GeneralString, GeneralValue)]
            position   = TPP.newPos (sourcefile ++ "(host: " ++ certname ++ ")") sourceline 1
        CResource <$> pure 0
                  <*> pure (Right utitle)
                  <*> fmap (T.unpack . T.toLower) (o .: "type")
                  <*> pure parameters
                  <*> pure DT.Normal
                  <*> pure position
    parseJSON _ = mzero

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
isNotLocal fqdn cr =
    let prms = crparams cr
        e    = filter ((== Right "EXPORTEDSOURCE") . fst ) prms :: [(GeneralString, GeneralValue)]
    in case e of
        [(_, Right (ResolvedString x))] -> x /= fqdn
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
