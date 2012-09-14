{-# LANGUAGE OverloadedStrings #-}

module PuppetDB.Rest where

import Puppet.Interpreter.Types

import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Traversable (traverse)
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

rawRequest :: IO (Maybe ResolvedValue)
rawRequest = do
        let q = BC.unpack $ W.renderSimpleQuery False [("query","[\"and\", [\"=\", \"type\", \"User\"]]")]
        initReq <- parseUrl ("http://localhost:8080/resources?" ++ q)
        let req = initReq { requestHeaders = [("Accept", "application/json")] }
        o <- withManager (\manager -> fmap responseBody $ httpLbs req manager) :: IO L.ByteString
        let utf8 = IConv.convert "LATIN1" "UTF-8" o
        return (decode' utf8 :: Maybe ResolvedValue)
