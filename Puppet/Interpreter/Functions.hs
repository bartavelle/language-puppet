module Puppet.Interpreter.Functions (fqdn_rand,regsubst) where

import Data.Hash.MD5
import Data.String.Utils (join)
import Text.RegexPR
import Puppet.Interpreter.Types
{-
TODO : achieve compatibility with puppet
the first String must be the fqdn
-}
fqdn_rand :: Integer -> [String] -> CatalogMonad Integer
fqdn_rand n args = return (hash `mod` n)
    where
        fullstring = join ":" args
        hash = md5i $ Str fullstring

regsubst :: String -> String -> String -> String -> CatalogMonad String
regsubst str src dst flags = do
    let multiline   = elem 'M' flags
        extended    = elem 'E' flags
        insensitive = elem 'I' flags
        global      = elem 'G' flags
        refunc | global = gsubRegexPR
               | otherwise = subRegexPR
    return $ refunc src dst str
