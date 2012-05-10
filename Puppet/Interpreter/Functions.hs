module Puppet.Interpreter.Functions (fqdn_rand) where

import Data.Hash.MD5
import Data.String.Utils (join)
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


