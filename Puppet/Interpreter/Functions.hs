module Puppet.Interpreter.Functions 
    (fqdn_rand
    ,regsubst
    ,mysql_password
    ,regmatch
    ,versioncmp
    ,file
    ,puppetSplit
    ,puppetSHA1
    ,puppetMD5
    ,generate
    ,pdbresourcequery
    ) where

import Prelude hiding (catch)
import Control.Exception
import PuppetDB.Rest
import Puppet.Printers
import Puppet.Interpreter.Types

import Data.Hash.MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS
import Data.String.Utils (join,replace)
import Text.RegexPR
import Text.Regex.PCRE.String
import Control.Monad.Error
import System.IO
import qualified Data.ByteString.Base16 as B16
import SafeProcess
import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toUpper)

puppetMD5  = md5s . Str
puppetSHA1 = BS.unpack . B16.encode . SHA1.hash . BS.pack
puppetMysql = BS.unpack . B16.encode . SHA1.hash . SHA1.hash . BS.pack

{-
TODO : achieve compatibility with puppet
the first String must be the fqdn
-}
fqdn_rand :: Integer -> [String] -> CatalogMonad Integer
fqdn_rand n args = return (hash `mod` n)
    where
        fullstring = Data.String.Utils.join ":" args
        hash = md5i $ Str fullstring

mysql_password :: String -> CatalogMonad String
mysql_password pwd = return $ '*':hash
    where
        hash = map toUpper $ puppetMysql pwd

regsubst :: String -> String -> String -> String -> CatalogMonad String
regsubst str src dst flags = do
    let multiline   = 'M' `elem` flags
        extended    = 'E' `elem` flags
        insensitive = 'I' `elem` flags
        global      = 'G' `elem` flags
        refunc | global = gsubRegexPR
               | otherwise = subRegexPR
    when multiline   $ throwError "Multiline flag not implemented"
    when extended    $ throwError "Extended flag not implemented"
    when insensitive $ throwError "Case insensitive flag not implemented"
    return $ refunc (alterregexp src) dst str
alterregexp :: String -> String
alterregexp = replace "\\n" "\n"

regmatch :: String -> String -> IO (Either String Bool)
regmatch str reg = do
    icmp <- compile compBlank execBlank reg
    case icmp of
        Right rr -> do
            x <- execute rr str
            case x of
                Right (Just _) -> return $ Right True
                Right Nothing  -> return $ Right False
                Left err -> return $ Left $ show err
        Left err -> return $ Left $ show err

-- TODO
versioncmp :: String -> String -> Integer
versioncmp a b | a > b = 1
               | a < b = -1
               | otherwise = 0

file :: [String] -> IO (Maybe String)
file [] = return Nothing
-- this is bad, is should be rewritten as a ByteString
file (x:xs) = catch (fmap Just (withFile x ReadMode (\fh -> do { y <- hGetContents fh; evaluate (length y); return y }))) ((\_ -> file xs) :: SomeException -> IO (Maybe String))

puppetSplit :: String -> String -> IO (Either String [String])
puppetSplit str reg = do
    icmp <- compile compBlank execBlank reg
    case icmp of
        Right rr -> execSplit rr str
        Left err -> return $ Left $ show err

-- helper for puppetSplit, once the regexp is compiled
execSplit :: Regex -> String -> IO (Either String [String])
execSplit _  ""  = return $ Right [""]
execSplit rr str = do
    x <- regexec rr str
    case x of
        Right (Just (before, _, after, _)) -> do
            sx <- execSplit rr after
            case sx of
                Right s -> return $ Right $ before:s
                Left er -> return $ Left  $ show er
        Right Nothing  -> return $ Right [str]
        Left err -> return $ Left $ show err

generate :: String -> [String] -> IO (Maybe String)
generate command args = do
    cmdout <- safeReadProcessTimeout command args (BSL.empty) 60000
    case cmdout of
        Just (Right x)  -> return $ Just (BSL.unpack x)
        _               -> return Nothing

pdbresourcequery :: String -> Maybe String -> CatalogMonad ResolvedValue
pdbresourcequery query key = do
    let
        extractSubHash :: String -> [ResolvedValue] -> Either String ResolvedValue
        extractSubHash k vals = let o = map (extractSubHash' k) vals
                                  in  if (null $ lefts o)
                                          then Right $ ResolvedArray $ rights o
                                          else Left $ "Something wrong happened while extracting the subhashes for key " ++ k ++ ": " ++ Data.List.intercalate ", " (lefts o)
        extractSubHash' :: String -> ResolvedValue -> Either String ResolvedValue
        extractSubHash' k (ResolvedHash hs) = let f = map snd $ filter ( (==k) . fst ) hs
                                                in  case f of
                                                        [o] -> Right o
                                                        []  -> Left "Key not found"
                                                        _   -> Left "More than one result, this is extremely bad."
        extractSubHash' _ x = Left $ "Expected a hash, not " ++ showValue x
    v <- liftIO $ rawRequest "http://localhost:8080" "resources" query
    rv <- case v of
        Right rh@(ResolvedArray _)  -> return rh
        Right wtf                   -> throwPosError $ "Expected an array from PuppetDB, not " ++ showValue wtf
        Left err                    -> throwPosError $ "Error during Puppet query: " ++ err
    case (key, rv) of
        (Nothing, _) -> return rv
        (Just k , ResolvedArray ar) -> case extractSubHash k ar of
                                               Right x -> return x
                                               Left  r -> throwPosError r
        _            -> throwPosError $ "Can't happen at pdbresourcequery"

