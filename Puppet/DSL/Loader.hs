module Puppet.DSL.Loader (parseFile) where

import Text.Parsec
import Puppet.DSL.Types
import Puppet.DSL.Parser
import Control.Monad.Error

parseFile :: FilePath -> ErrorT String IO [Statement]
parseFile fpath = do
    result <- liftIO $ readFile fpath
    case (runParser mparser () fpath result) of
        Left err -> throwError (show err)
        Right st -> return st
