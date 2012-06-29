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
    ) where

import Data.Hash.MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS
import Data.String.Utils (join,replace)
import Text.RegexPR
import Puppet.Interpreter.Types
import Control.Monad.Error
import System.IO
import qualified Data.ByteString.Base16 as B16

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

regmatch :: String -> String -> Bool
regmatch str reg = case matchRegexPR (alterregexp str) reg of
    Just _  -> True
    Nothing -> False

-- TODO
versioncmp :: String -> String -> Integer
versioncmp a b | a > b = 1
               | a < b = -1
               | otherwise = 0

file :: [String] -> IO (Maybe String)
file [] = return Nothing
file (x:xs) = catch (liftM Just (withFile x ReadMode hGetContents)) (\_ -> file xs)

puppetSplit :: String -> String -> [String]
puppetSplit str reg = splitRegexPR reg str
