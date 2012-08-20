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
    ) where

import Data.Hash.MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS
import Data.String.Utils (join,replace)
import Text.RegexPR
import Text.Regex.PCRE.String
import Puppet.Interpreter.Types
import Control.Monad.Error
import System.IO
import qualified Data.ByteString.Base16 as B16
import SafeProcess

puppetMD5  = md5s . Str
puppetSHA1 = BS.unpack . B16.encode . SHA1.hash . BS.pack

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
        hash = puppetSHA1 pwd

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
file (x:xs) = catch (liftM Just (withFile x ReadMode hGetContents)) (\_ -> file xs)

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
    cmdout <- safeReadProcessTimeout command args "" 60000
    case cmdout of
        Just (Right x)  -> return $ Just x
        _               -> return Nothing
