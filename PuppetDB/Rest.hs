{-# LANGUAGE OverloadedStrings #-}

module PuppetDB.Rest where

import Puppet.Interpreter.Types
import Puppet.Printers

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

rawRequest :: String -> String -> String -> IO (Either String ResolvedValue)
rawRequest url querytype query = do
        let q = BC.unpack $ W.renderSimpleQuery False [("query", BC.pack query)]
            pfunc = parseUrl (url ++ "/" ++ querytype ++ "?" ++ q)
        eInitReq <- case querytype of
                       "resources"  -> fmap Right pfunc
                       "nodes"      -> fmap Right pfunc
                       _            -> return $ Left $ "Invalid query type " ++ querytype
        case eInitReq of
            Right initReq -> do
                let req = initReq { requestHeaders = [("Accept", "application/json")] }
                o <- withManager (\manager -> fmap responseBody $ httpLbs req manager) :: IO L.ByteString
                let utf8 = IConv.convert "LATIN1" "UTF-8" o
                case decode' utf8 :: Maybe ResolvedValue of
                    Just x@(ResolvedArray _) -> return $ Right x
                    Just x                   -> return $ Left $ "PuppetDB should have returned an array, not " ++ showValue x
                    Nothing                  -> return $ Left "Json decoding has failed"
            Left err -> return $ Left err
