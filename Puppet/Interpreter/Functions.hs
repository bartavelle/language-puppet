module Puppet.Interpreter.Functions
    ( fqdn_rand
    , regsubst
    , mysql_password
    , regmatch
    , versioncmp
    , file
    , puppetSplit
    , puppetSHA1
    , puppetMD5
    , generate
    , pdbresourcequery
    ) where

import PuppetDB.Query
import Puppet.Printers
import Puppet.Interpreter.Types
import Puppet.Utils

import Control.Monad.State
import Prelude hiding (catch)
import Control.Exception
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.MD5 as MD5
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import SafeProcess
import Data.Either (lefts, rights)
import Data.List (intercalate,foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Bits

puppetMD5 :: T.Text -> T.Text
puppetMD5   = T.decodeUtf8 . B16.encode . MD5.hash  . T.encodeUtf8
puppetSHA1  = T.decodeUtf8 . B16.encode . SHA1.hash . T.encodeUtf8
puppetMysql = T.decodeUtf8 . B16.encode . SHA1.hash . SHA1.hash . T.encodeUtf8

{-
TODO : achieve compatibility with puppet
the first String must be the fqdn
-}
fqdn_rand :: Integer -> [T.Text] -> CatalogMonad Integer
fqdn_rand n args = return (hash `mod` n)
    where
        fullstring = T.intercalate ":" args
        hash = foldl' (\x y -> x*10 + fromIntegral y) 0 $ BS.unpack $ BS.take 8 $ SHA1.hash $ T.encodeUtf8 fullstring

mysql_password :: T.Text -> CatalogMonad T.Text
mysql_password pwd = return $ T.cons '*' hash
    where
        hash = T.toUpper $ puppetMysql pwd

regsubst :: T.Text -> T.Text -> T.Text -> T.Text -> CatalogMonad T.Text
regsubst str reg dst flags = do
    let multiline   = if 'M' `textElem` flags then compMultiline else compBlank
        extended    = if 'E' `textElem` flags then compExtended  else compBlank
        insensitive = if 'I' `textElem` flags then compCaseless  else compBlank
        global      = 'G' `textElem` flags -- TODO fix global
        options = multiline .|. extended .|. insensitive
    regexp <- liftIO $ compile options execBlank (T.encodeUtf8 reg)
    case regexp of
        Left rr -> throwPosError (tshow rr)
        Right cr -> do
            res <- liftIO $ substitute cr (T.encodeUtf8 str) (T.encodeUtf8 dst)
            case res of
                Right r -> return (T.decodeUtf8 r)
                Left rr -> throwPosError (T.pack rr)


-- TODO
versioncmp :: T.Text -> T.Text -> Integer
versioncmp a b | a > b = 1
               | a < b = -1
               | otherwise = 0

file :: [T.Text] -> IO (Maybe T.Text)
file [] = return Nothing
-- this is bad, is should be rewritten as a ByteString
file (x:xs) = catch
    (fmap Just (T.readFile (T.unpack x)))
    (\SomeException{} -> file xs)

puppetSplit :: T.Text -> T.Text -> IO (Either String [T.Text])
puppetSplit str reg = fmap (fmap (map T.decodeUtf8)) (splitCompile (T.encodeUtf8 reg) (T.encodeUtf8 str))

generate :: T.Text -> [T.Text] -> IO (Maybe T.Text)
generate command args = do
    cmdout <- safeReadProcessTimeout (T.unpack command) (map T.unpack args) (TL.empty) 60000
    case cmdout of
        Just (Right x)  -> return $ Just x
        _               -> return Nothing

pdbresourcequery :: Query -> Maybe T.Text -> CatalogMonad ResolvedValue
pdbresourcequery query key = do
    let
        extractSubHash :: T.Text -> [ResolvedValue] -> Either String ResolvedValue
        extractSubHash k vals = let o = map (extractSubHash' k) vals
                                  in  if (null $ lefts o)
                                          then Right $ ResolvedArray $ rights o
                                          else Left $ "Something wrong happened while extracting the subhashes for key " ++ T.unpack k ++ ": " ++ Data.List.intercalate ", " (lefts o)
        extractSubHash' :: T.Text -> ResolvedValue -> Either String ResolvedValue
        extractSubHash' k (ResolvedHash hs) = let f = map snd $ filter ( (==k) . fst ) hs
                                                in  case f of
                                                        [o] -> Right o
                                                        []  -> Left "Key not found"
                                                        _   -> Left "More than one result, this is extremely bad."
        extractSubHash' _ x = Left $ "Expected a hash, not " ++ T.unpack (showValue x)
    qf <- fmap puppetDBFunction get
    v <- liftIO (qf "resources" query) >>= \r -> case r of
                                                     Left rr -> throwPosError (T.pack rr)
                                                     Right x -> return x
    --v <- liftIO $ rawRequest "http://localhost:8080" "resources" query
    rv <- case json2puppet v of
        Right rh@(ResolvedArray _)  -> return rh
        Right wtf                   -> throwPosError $ "Expected an array from PuppetDB, not " <> showValue wtf
        Left err                    -> throwPosError $ "Error during Puppet query: " <> T.pack err
    case (key, rv) of
        (Nothing, _) -> return rv
        (Just k , ResolvedArray ar) -> case extractSubHash k ar of
                                               Right x -> return x
                                               Left  r -> throwPosError (T.pack r)
        _            -> throwPosError $ "Can't happen at pdbresourcequery"

regmatch :: T.Text -> T.Text -> IO (Either String Bool)
regmatch str reg = do
    icmp <- compile compBlank execBlank (T.encodeUtf8 reg)
    case icmp of
        Right rr -> do
            x <- execute rr (T.encodeUtf8 str)
            case x of
                Right (Just _) -> return $ Right True
                Right Nothing  -> return $ Right False
                Left err -> return $ Left $ show err
        Left err -> return $ Left $ show err

