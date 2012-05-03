module Puppet.Interpreter.Catalog (
    getCatalog
    ) where

import Puppet.Preferences
import Puppet.DSL.Types
import Puppet.Interpreter.Types

getCatalog :: (TopLevelType -> String -> IO (Either String Statement)) -> String -> Facts -> IO (Either String Catalog)
getCatalog getstatements nodename facts = do
    nodestmt <- getstatements TopNode nodename
    case nodestmt of
        Left x -> return $ Left x
        Right y -> return $ Right []

