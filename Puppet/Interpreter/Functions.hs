module Puppet.Interpreter.Functions 
    (fqdn_rand
    ,regsubst
    ,mysql_password
    ,regmatch
    ,versioncmp
    ,file
    ) where

import Data.Hash.MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS
import Data.String.Utils (join)
import Text.RegexPR
import Puppet.Interpreter.Types
import Control.Monad.Error
import System.IO
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
        hash = BS.unpack $ SHA1.hash (BS.pack pwd)

regsubst :: String -> String -> String -> String -> CatalogMonad String
regsubst str src dst flags = do
    let multiline   = elem 'M' flags
        extended    = elem 'E' flags
        insensitive = elem 'I' flags
        global      = elem 'G' flags
        refunc | global = gsubRegexPR
               | otherwise = subRegexPR
    if multiline
        then throwError "Multiline flag not implemented"
        else return ()
    if extended
        then throwError "Extended flag not implemented"
        else return ()
    if insensitive
        then throwError "Case insensitive flag not implemented"
        else return ()
    return $ refunc src dst str

regmatch :: String -> String -> Bool
regmatch str reg = case matchRegexPR str reg of
    Just _  -> True
    Nothing -> False

-- TODO
versioncmp :: String -> String -> Integer
versioncmp a b | a > b = 1
               | a < b = -1
               | otherwise = 0

file :: [String] -> IO (Maybe String)
file [] = return Nothing
file (x:xs) = catch (withFile x ReadMode hGetContents >>= return . Just) (\_ -> file xs)
